// Copyright 2024 the Vello Authors
// Copyright 2025 the Tabulon Authors
// SPDX-License-Identifier: Apache-2.0 OR MIT

//! Tabulon DXF viewer
//!
//! Drag and drop DXF (text or binary format) files onto the viewer window to open them, or provide
//! a path on the command line.
//!
//! ## Limitations
//!
//! This viewer is focused exclusively on 2D vector drawing content inside DXFs, and does not render
//! 3D content.
//!
//! Here is a list of entities supported in their simple plotter/solid style, facing +Z:
//!
//! - `ARC`
//! - `LINE`
//! - `CIRCLE`
//! - `ELLIPSE`
//! - `LWPOLYLINE`
//! - `POLYLINE`
//! - `SPLINE`
//! - `SOLID`
//! - `TEXT`
//! - `MTEXT`
//!
//! In `TEXT` and `MTEXT`, inline style definitions are not currently supported, and not all special
//! character escapes are translated properly or at all.
//!
//! In general, this viewer is quite scalable, but will struggle when the density of elements in a
//! given position is high enough, due to limitations in the classic Vello rendering process.

#![windows_subsystem = "windows"]

use anyhow::Result;
use joto_constants::u64::{INCH, MICROMETER};
use std::num::NonZeroUsize;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::thread;
use std::time::Instant;
use tracing_subscriber::prelude::*;
use ui_events::{
    ScrollDelta,
    pointer::{PointerButton, PointerEvent, PointerId, PointerInfo, PointerType, PointerUpdate},
};
use ui_events_winit::{WindowEventReducer, WindowEventTranslation};
use vello::kurbo::{
    Affine, DEFAULT_ACCURACY, ParamCurveNearest, PathSeg, Point, Rect, Shape, Stroke, Vec2,
};
use vello::peniko::{Brush, Color, color::palette};
use vello::util::{RenderContext, RenderSurface};
use vello::{AaConfig, Renderer, RendererOptions, Scene};
use winit::application::ApplicationHandler;
use winit::dpi::LogicalSize;
use winit::event::WindowEvent;
use winit::event_loop::{ActiveEventLoop, EventLoop};
use winit::window::Window;

use vello::wgpu;

use tabulon_dxf::{EntityHandle, RestrokePaint, TDDrawing};

use tabulon::{
    GraphicsBag, GraphicsItem, ItemHandle, PaintHandle,
    render_layer::RenderLayer,
    shape::{FatPaint, FatShape},
};

extern crate alloc;

use alloc::collections::{BTreeMap, BTreeSet};

enum RenderState<'s> {
    /// `RenderSurface` and `Window` for active rendering.
    Active {
        // The `RenderSurface` and the `Window` must be in this order, so that the surface is dropped first.
        surface: Box<RenderSurface<'s>>,
        window: Arc<Window>,
    },
    /// Cache a window so that it can be reused when the app is resumed after being suspended.
    Suspended(Option<Arc<Window>>),
}

#[derive(Default)]
struct GestureState {
    /// Pointer currently panning.
    pan: Option<PointerId>,
    /// Cursor position.
    cursor_pos: Point,
}

struct DrawingViewer {
    /// `tabulon_dxf` drawing.
    td: TDDrawing,

    /// Index of bounding boxes for hit testing.
    picking_index: EntityIndex,
    /// Which shape is closest to the cursor?
    pick: Option<EntityHandle>,

    /// Index of bounding boxes for culling texts.
    text_cull_index: TextCullIndex,

    /// View transform of the drawing.
    view_transform: Affine,
    /// View scale of the drawing.
    view_scale: f64,

    /// Defer reprojection until after redraw is completed.
    defer_reprojection: bool,

    /// State of gesture processing (e.g. panning, zooming).
    gestures: GestureState,
}

struct TabulonDxfViewer<'s> {
    /// The vello `RenderContext` which is a global context that lasts for the lifetime of the application.
    context: RenderContext,

    /// An array of renderers, one per wgpu device.
    renderers: Vec<Option<Renderer>>,

    /// The window, and also the surface while actively rendering.
    state: RenderState<'s>,

    /// A vello Scene which is a data structure which allows one to build up a description a scene to be
    /// drawn (with paths, fills, images, text, etc) which is then passed to a renderer for rendering.
    scene: Scene,

    /// Tabulon Vello environment.
    tv_environment: tabulon_vello::Environment,

    /// ui-events `WindowEvent` reducer.
    event_reducer: WindowEventReducer,

    /// State related to viewing a specific drawing.
    viewer: Option<DrawingViewer>,

    /// Handles for threads loading hovered files.
    hover_threads: BTreeMap<PathBuf, thread::JoinHandle<Result<TDDrawing>>>,
}

impl ApplicationHandler for TabulonDxfViewer<'_> {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        let RenderState::Suspended(cached_window) = &mut self.state else {
            return;
        };

        let window = cached_window
            .take()
            .unwrap_or_else(|| create_winit_window(event_loop));

        // Create a vello Surface.
        let size = window.inner_size();
        let surface_future = {
            let surface = self
                .context
                .instance
                .create_surface(wgpu::SurfaceTarget::from(window.clone()))
                .expect("Error creating surface");
            let dev_id = pollster::block_on(self.context.device(Some(&surface)))
                .expect("No compatible device");
            let device_handle = &self.context.devices[dev_id];
            let capabilities = surface.get_capabilities(device_handle.adapter());
            let present_mode = if capabilities
                .present_modes
                .contains(&wgpu::PresentMode::Mailbox)
            {
                wgpu::PresentMode::Mailbox
            } else {
                wgpu::PresentMode::AutoVsync
            };
            self.context
                .create_render_surface(surface, size.width, size.height, present_mode)
        };

        let scale_factor = window.scale_factor();

        let surface = pollster::block_on(surface_future).expect("Error creating surface");

        // Create a vello Renderer for the surface (using its device id).
        self.renderers
            .resize_with(self.context.devices.len(), || None);
        self.renderers[surface.dev_id]
            .get_or_insert_with(|| create_vello_renderer(&self.context, &surface));

        if let Some(path_arg) = std::env::args().next_back() {
            match load_drawing(&path_arg) {
                Ok(mut drawing) => {
                    let mut title = String::from("Tabulon DXF Viewer — ");
                    title.push_str(
                        Path::new(&path_arg)
                            .file_name()
                            .unwrap_or_default()
                            .to_str()
                            .unwrap_or_default(),
                    );
                    window.set_title(&title);

                    let picking_index = EntityIndex::new(&drawing);
                    let bounds = picking_index.bounds();

                    let text_cull_index = TextCullIndex::new(&mut self.tv_environment, &drawing);

                    let mut scene = Scene::default();
                    let view_scale = (size.height as f64 / bounds.size().height)
                        .min(size.width as f64 / bounds.size().width);

                    let view_transform = Affine::translate(Vec2 {
                        x: -bounds.min_x(),
                        y: -bounds.min_y(),
                    })
                    .then_scale(view_scale);
                    update_transform(
                        &mut drawing.graphics,
                        drawing.restroke_paints.clone(),
                        view_transform,
                        view_scale,
                        scale_factor,
                    );
                    self.scene.reset();

                    let encode_started = Instant::now();
                    self.tv_environment.add_render_layer_to_scene(
                        &mut scene,
                        &drawing.graphics,
                        &drawing.render_layer,
                    );
                    let encode_duration = Instant::now().saturating_duration_since(encode_started);
                    eprintln!("Initial projection/encode took {encode_duration:?}");

                    self.viewer = Some(DrawingViewer {
                        td: drawing,
                        picking_index,
                        view_scale,
                        view_transform,
                        text_cull_index,
                        gestures: GestureState::default(),
                        defer_reprojection: false,
                        pick: None,
                    });
                }
                Err(e) => {
                    tracing::error!("Failed to load drawing: {e}");
                }
            }
        }

        // Save the Window and Surface to a state variable.
        self.state = RenderState::Active {
            surface: Box::new(surface),
            window,
        };
    }

    fn suspended(&mut self, _event_loop: &ActiveEventLoop) {
        if let RenderState::Active { window, .. } = &self.state {
            self.state = RenderState::Suspended(Some(window.clone()));
        }
    }

    #[tracing::instrument(skip_all)]
    fn window_event(
        &mut self,
        event_loop: &ActiveEventLoop,
        window_id: winit::window::WindowId,
        event: WindowEvent,
    ) {
        let (surface, window) = match &mut self.state {
            RenderState::Active { surface, window } if window.id() == window_id => {
                (surface, window)
            }
            _ => return,
        };

        let mut reproject = false;
        // Set if reprojection is requested as a result of a deferral.
        let mut reproject_deferred = false;

        if !matches!(
            event,
            WindowEvent::KeyboardInput {
                is_synthetic: true,
                ..
            }
        ) {
            if let Some(wet) = self.event_reducer.reduce(&event) {
                match wet {
                    WindowEventTranslation::Keyboard(k) => {
                        use ui_events::keyboard::{Key, NamedKey};
                        if k.state.is_down() && matches!(k.key, Key::Named(NamedKey::Escape)) {
                            event_loop.exit();
                        }
                    }
                    WindowEventTranslation::Pointer(p) => {
                        let Some(viewer) = &mut self.viewer else {
                            return;
                        };

                        match p {
                            PointerEvent::Down {
                                pointer:
                                    PointerInfo {
                                        pointer_id,
                                        pointer_type: PointerType::Mouse,
                                        ..
                                    },
                                button: Some(PointerButton::Primary),
                                state,
                            }
                            | PointerEvent::Down {
                                pointer:
                                    PointerInfo {
                                        pointer_id,
                                        pointer_type: PointerType::Touch,
                                        ..
                                    },
                                state,
                                ..
                            } => {
                                if viewer.gestures.pan.is_none() {
                                    viewer.gestures.pan = pointer_id;
                                    viewer.gestures.cursor_pos = Point {
                                        x: state.position.x,
                                        y: state.position.y,
                                    }
                                }
                            }
                            PointerEvent::Move(PointerUpdate {
                                pointer: PointerInfo { pointer_id, .. },
                                current,
                                ..
                            }) => {
                                let p = Point {
                                    x: current.position.x,
                                    y: current.position.y,
                                };

                                let dp = viewer.view_transform.inverse() * p;

                                if viewer.gestures.pan == pointer_id {
                                    viewer.view_transform = viewer
                                        .view_transform
                                        .then_translate(-(viewer.gestures.cursor_pos - p));
                                    reproject = true;
                                } else if pointer_id == Some(PointerId::PRIMARY) {
                                    let pick_dist: f64 = window.scale_factor() * 1.414;
                                    let pick_started = Instant::now();

                                    let pick = viewer
                                        .picking_index
                                        .pick(dp, pick_dist * viewer.view_scale.recip());

                                    if viewer.pick != pick {
                                        if let Some(pick) = pick {
                                            let pick_duration = Instant::now()
                                                .saturating_duration_since(pick_started);
                                            eprintln!(
                                                "{:#?}",
                                                viewer.td.info.get_entity(pick).specific
                                            );
                                            eprintln!("Pick took {pick_duration:?}");
                                        }
                                        viewer.pick = pick;
                                        reproject = true;
                                    }
                                }

                                viewer.gestures.cursor_pos = p;
                            }
                            PointerEvent::Up {
                                pointer: PointerInfo { pointer_id, .. },
                                ..
                            }
                            | PointerEvent::Cancel(PointerInfo { pointer_id, .. }) => {
                                if viewer.gestures.pan == pointer_id {
                                    viewer.gestures.pan = None;
                                }
                            }
                            PointerEvent::Scroll { delta, .. } => {
                                let d = match delta {
                                    ScrollDelta::LineDelta(_, y) => y as f64 * 0.1,
                                    ScrollDelta::PixelDelta(pd) => pd.y * 0.05,
                                    _ => 0.,
                                };

                                viewer.view_transform = viewer
                                    .view_transform
                                    .then_scale_about(1. + d, viewer.gestures.cursor_pos);
                                viewer.view_scale *= 1. + d;
                                reproject = true;
                            }
                            _ => {}
                        }
                    }
                }
            }
        }

        match event {
            WindowEvent::CloseRequested => event_loop.exit(),

            WindowEvent::Resized(size) => {
                self.context
                    .resize_surface(surface, size.width, size.height);
            }

            WindowEvent::HoveredFileCancelled => {
                self.hover_threads.clear();
            }

            WindowEvent::HoveredFile(p) => {
                let pb = p.clone();
                if let Ok(jh) = thread::Builder::new().spawn(move || load_drawing(&pb)) {
                    self.hover_threads.insert(p, jh);
                }
            }

            WindowEvent::DroppedFile(p) => {
                let jh = self.hover_threads.remove(&p).unwrap_or_else(|| {
                    let pb = p.clone();
                    thread::Builder::new()
                        .spawn(move || load_drawing(&pb))
                        .unwrap()
                });

                let Ok(Ok(drawing)) = jh.join() else {
                    return;
                };

                let mut title = String::from("Tabulon DXF Viewer — ");
                title.push_str(
                    p.file_name()
                        .unwrap_or_default()
                        .to_str()
                        .unwrap_or_default(),
                );
                window.set_title(&title);

                let picking_index = EntityIndex::new(&drawing);
                let bounds = picking_index.bounds();

                let text_cull_index = TextCullIndex::new(&mut self.tv_environment, &drawing);

                let view_scale = (surface.config.height as f64 / bounds.size().height)
                    .min(surface.config.width as f64 / bounds.size().width);

                let view_transform = Affine::translate(Vec2 {
                    x: -bounds.min_x(),
                    y: -bounds.min_y(),
                })
                .then_scale(view_scale);

                self.viewer = Some(DrawingViewer {
                    td: drawing,
                    picking_index,
                    view_scale,
                    view_transform,
                    text_cull_index,
                    pick: None,
                    gestures: GestureState::default(),
                    defer_reprojection: false,
                });

                reproject = true;
            }

            WindowEvent::PinchGesture { delta: d, .. } => {
                let Some(viewer) = &mut self.viewer else {
                    return;
                };

                viewer.view_transform = viewer
                    .view_transform
                    .then_translate(-viewer.gestures.cursor_pos.to_vec2())
                    .then_scale(1. + d)
                    .then_translate(viewer.gestures.cursor_pos.to_vec2());
                viewer.view_scale *= 1. + d;
                reproject = true;
            }

            WindowEvent::RedrawRequested => {
                let wgpu::SurfaceConfiguration { width, height, .. } = surface.config;

                let device_handle = &self.context.devices[surface.dev_id];

                let surface_texture = tracing::info_span!("get_current_texture").in_scope(|| {
                    surface
                        .surface
                        .get_current_texture()
                        .expect("failed to get surface texture")
                });

                tracing::info_span!("render_to_texture").in_scope(|| {
                    // Render to the surface's texture
                    self.renderers[surface.dev_id]
                        .as_mut()
                        .unwrap()
                        .render_to_texture(
                            &device_handle.device,
                            &device_handle.queue,
                            &self.scene,
                            &surface.target_view,
                            &vello::RenderParams {
                                base_color: Color::WHITE, // Background color
                                width,
                                height,
                                antialiasing_method: AaConfig::Area,
                            },
                        )
                        .expect("failed to render to the texture");
                });

                tracing::info_span!("texture_surface_blit").in_scope(|| {
                    let mut encoder = device_handle.device.create_command_encoder(
                        &wgpu::CommandEncoderDescriptor {
                            label: Some("Surface Blit"),
                        },
                    );
                    surface.blitter.copy(
                        &device_handle.device,
                        &mut encoder,
                        &surface.target_view,
                        &surface_texture
                            .texture
                            .create_view(&wgpu::TextureViewDescriptor::default()),
                    );
                    device_handle.queue.submit([encoder.finish()]);
                });

                tracing::info_span!("present_surface").in_scope(|| {
                    surface_texture.present();
                });

                #[cfg(feature = "tracing-tracy")]
                tracy_client::frame_mark();

                device_handle.device.poll(wgpu::Maintain::Poll);

                if let Some(viewer) = &self.viewer {
                    if viewer.defer_reprojection {
                        reproject_deferred = true;
                    }
                };
            }
            _ => {}
        }

        if reproject || reproject_deferred {
            tracing::info_span!("reproject").in_scope(|| {
                let Some(viewer) = &mut self.viewer else {
                    return;
                };
                if reproject_deferred {
                    viewer.defer_reprojection = false;
                }
                if viewer.defer_reprojection {
                    return;
                }
                // direct requests for reprojection until after the next redraw is complete.
                viewer.defer_reprojection = reproject;
                let reproject_started = Instant::now();
                update_transform(
                    &mut viewer.td.graphics,
                    viewer.td.restroke_paints.clone(),
                    viewer.view_transform,
                    viewer.view_scale,
                    window.scale_factor(),
                );

                let tl = viewer.view_transform.inverse() * Point { x: 0., y: 0. };
                let br = viewer.view_transform.inverse()
                    * Point {
                        x: surface.config.width as f64,
                        y: surface.config.height as f64,
                    };

                #[allow(
                    clippy::cast_possible_truncation,
                    reason = "The loss of range and precision is acceptable."
                )]
                let visible = viewer.picking_index.query_items(
                    tl.x as f32,
                    tl.y as f32,
                    br.x as f32,
                    br.y as f32,
                );

                #[allow(
                    clippy::cast_possible_truncation,
                    reason = "The loss of range and precision is acceptable."
                )]
                let visible_text = viewer.text_cull_index.query_items(
                    tl.x as f32,
                    tl.y as f32,
                    br.x as f32,
                    br.y as f32,
                );

                let culled_render_layer =
                    viewer
                        .td
                        .render_layer
                        .filter(|ih| match viewer.td.graphics.get(*ih) {
                            GraphicsItem::FatShape(..) => visible.binary_search(ih).is_ok(),
                            GraphicsItem::FatText(..) => visible_text.contains(ih),
                        });
                self.scene.reset();
                self.tv_environment.add_render_layer_to_scene(
                    &mut self.scene,
                    &viewer.td.graphics,
                    &culled_render_layer,
                );

                if let Some(pick) = viewer.pick {
                    let mut gb = GraphicsBag::default();
                    let mut rl = RenderLayer::default();

                    gb.update_transform(Default::default(), viewer.view_transform);

                    let paint = gb.register_paint(FatPaint {
                        stroke: Stroke::new(1.414 / viewer.view_scale),
                        stroke_paint: Some(palette::css::GOLDENROD.into()),
                        fill_paint: None,
                    });

                    culled_render_layer
                        .indices
                        .iter()
                        .filter(|ih| viewer.td.item_entity_map[ih] == pick)
                        .for_each(|ih| {
                            let GraphicsItem::FatShape(FatShape {
                                transform, path, ..
                            }) = viewer.td.graphics.get(*ih)
                            else {
                                return;
                            };
                            rl.push_with_bag(
                                &mut gb,
                                FatShape {
                                    transform: *transform,
                                    path: path.clone(),
                                    paint,
                                },
                            );
                        });

                    self.tv_environment
                        .add_render_layer_to_scene(&mut self.scene, &gb, &rl);
                }

                let reproject_duration =
                    Instant::now().saturating_duration_since(reproject_started);
                eprintln!("Reprojection/reencoding took {reproject_duration:?}");

                window.request_redraw();
            });
        }
    }
}

/// Load a drawing file into a drawing, and print some stats.
fn load_drawing(p: impl AsRef<Path>) -> Result<TDDrawing> {
    let drawing_load_started = Instant::now();
    let mut drawing = tabulon_dxf::load_file_default_layers(p)?;

    let drawing_load_duration = Instant::now().saturating_duration_since(drawing_load_started);
    eprintln!("Drawing took {drawing_load_duration:?} to load and translate.");

    light_adapt_paints(&mut drawing.graphics, &drawing.render_layer);

    {
        let mut segment_count = 0;
        let mut text_count = 0;
        for item_handle in drawing.item_entity_map.keys() {
            match drawing.graphics.get(*item_handle) {
                GraphicsItem::FatShape(FatShape { path, .. }) => {
                    segment_count += path.segments().count();
                }
                GraphicsItem::FatText(_) => text_count += 1,
            }
        }
        eprintln!(
            "Loaded {} unique entities, {} path segments, {} text blocks.",
            drawing.item_entity_map.len(),
            segment_count,
            text_count
        );
        let linewidths: BTreeSet<u64> = drawing.restroke_paints.iter().map(|r| r.weight).collect();
        eprintln!(
            "There are {} unique linewidths, between {} µm and {} µm.",
            linewidths.len(),
            linewidths.first().unwrap() / MICROMETER,
            linewidths.last().unwrap() / MICROMETER,
        );
    }

    Ok(drawing)
}

#[cfg(feature = "tracing-tracy-memory")]
#[global_allocator]
static GLOBAL: tracy_client::ProfiledAllocator<std::alloc::System> =
    tracy_client::ProfiledAllocator::new(std::alloc::System, 100);

fn main() -> Result<()> {
    let subscriber = tracing_subscriber::registry()
        .with(tracing_subscriber::fmt::layer())
        .with(
            tracing_subscriber::EnvFilter::builder()
                .with_default_directive(tracing::level_filters::LevelFilter::WARN.into())
                .from_env_lossy(),
        );

    #[cfg(feature = "tracing-tracy")]
    let tracy_layer = tracing_tracy::TracyLayer::default();
    #[cfg(feature = "tracing-tracy")]
    let subscriber = subscriber.with(tracy_layer);

    subscriber.init();

    let mut app = TabulonDxfViewer {
        context: RenderContext::new(),
        renderers: vec![],
        state: RenderState::Suspended(None),
        scene: Scene::new(),
        tv_environment: Default::default(),
        event_reducer: Default::default(),
        viewer: None,
        hover_threads: Default::default(),
    };

    let event_loop = EventLoop::new()?;
    event_loop
        .run_app(&mut app)
        .expect("Couldn't run event loop");
    Ok(())
}

/// Helper function that creates a Winit window and returns it (wrapped in an Arc for sharing between threads)
fn create_winit_window(event_loop: &ActiveEventLoop) -> Arc<Window> {
    let attr = Window::default_attributes()
        .with_inner_size(LogicalSize::new(960, 720))
        .with_resizable(true)
        .with_title("Tabulon DXF Viewer");
    Arc::new(event_loop.create_window(attr).unwrap())
}

/// Helper function that creates a vello `Renderer` for a given `RenderContext` and `RenderSurface`
fn create_vello_renderer(render_cx: &RenderContext, surface: &RenderSurface<'_>) -> Renderer {
    Renderer::new(
        &render_cx.devices[surface.dev_id].device,
        RendererOptions {
            use_cpu: false,
            antialiasing_support: vello::AaSupport::area_only(),
            num_init_threads: NonZeroUsize::new(1),
            pipeline_cache: None,
        },
    )
    .expect("Couldn't create renderer")
}

/// Update the transform/scale in all the items in a `GraphicsBag`.
///
/// This also adapts line widths from the drawing so they are the correct
/// size after scaling.
#[tracing::instrument(skip_all)]
fn update_transform(
    graphics: &mut GraphicsBag,
    restroke_paints: Arc<[RestrokePaint]>,
    transform: Affine,
    view_scale: f64,
    scale_factor: f64,
) {
    // Update root transform.
    graphics.update_transform(Default::default(), transform);

    // Update default stroke.
    graphics.update_paint(
        Default::default(),
        FatPaint {
            // Unfortunately, post-transform stroke widths are not supported.
            stroke: Stroke::new(1.0 / view_scale),
            stroke_paint: Some(Color::BLACK.into()),
            fill_paint: None,
        },
    );

    #[allow(clippy::cast_possible_truncation, reason = "Deliberate truncation.")]
    let pixel_pitch = INCH / (96_f64 * scale_factor).trunc() as u64;

    for r in restroke_paints.iter() {
        r.adapt(graphics, pixel_pitch, view_scale, 1.0, f64::INFINITY);
    }
}

/// Light adapt paints.
///
/// The ACI palette and drawings using it assume a black background,
/// this adapts colors to have a reasonable degree of contrast for the
/// time being, until a more permanent solution is found.
fn light_adapt_paints(graphics: &mut GraphicsBag, render_layer: &RenderLayer) {
    let paint_handles: BTreeSet<PaintHandle> = render_layer
        .indices
        .iter()
        .map(|ih| match graphics.get(*ih) {
            GraphicsItem::FatShape(s) => s.paint,
            GraphicsItem::FatText(t) => t.paint,
        })
        .collect();

    for handle in paint_handles {
        let p = graphics.get_paint_mut(handle);
        if let Some(Brush::Solid(c)) = p.stroke_paint {
            p.stroke_paint = Some(Brush::Solid(c.map_lightness(|x| 1.2 - x)));
        }
        if let Some(Brush::Solid(c)) = p.fill_paint {
            p.fill_paint = Some(Brush::Solid(c.map_lightness(|x| 1.2 - x)));
        }
    }
}

use static_aabb2d_index::{StaticAABB2DIndex, StaticAABB2DIndexBuilder};

/// Bounding box index for entities.
struct EntityIndex {
    bounds_index: StaticAABB2DIndex<f32>,
    lines: Box<[PathSeg]>,
    entity_mapping: Box<[EntityHandle]>,
    item_mapping: Box<[ItemHandle]>,
}

#[allow(
    clippy::cast_possible_truncation,
    reason = "The loss of range and precision is acceptable."
)]
impl EntityIndex {
    fn new(d: &TDDrawing) -> Self {
        let build_started = Instant::now();

        let mut lines: Vec<PathSeg> = vec![];
        let mut entity_mapping = vec![];
        let mut item_mapping = vec![];
        for (k, v) in d.item_entity_map.iter() {
            let GraphicsItem::FatShape(FatShape { path, .. }) = d.graphics.get(*k) else {
                continue;
            };

            for seg in path.segments() {
                entity_mapping.push(*v);
                item_mapping.push(*k);
                lines.push(seg);
            }
        }
        let lines = Box::from(lines.as_slice());
        let entity_mapping = Box::from(entity_mapping.as_slice());
        let item_mapping = Box::from(item_mapping.as_slice());

        let bounds_index = compute_bounds_index(&lines);

        let build_duration = Instant::now().saturating_duration_since(build_started);
        eprintln!("Bounds index took {build_duration:?} to build.");

        Self {
            bounds_index,
            lines,
            entity_mapping,
            item_mapping,
        }
    }

    /// Pick entity that is closest to dp.
    #[tracing::instrument(skip_all)]
    fn pick(&self, dp: Point, sp: f64) -> Option<EntityHandle> {
        self.bounds_index
            .query(
                (dp.x - sp) as f32,
                (dp.y - sp) as f32,
                (dp.x + sp) as f32,
                (dp.y + sp) as f32,
            )
            .into_iter()
            .fold((f64::INFINITY, None), |(dsq, i), b| {
                let ndsq = self.lines[b].nearest(dp, DEFAULT_ACCURACY).distance_sq;
                if ndsq < dsq && ndsq < (sp * sp) {
                    (ndsq, Some(b))
                } else {
                    (dsq, i)
                }
            })
            .1
            .map(|i| self.entity_mapping[i])
    }

    /// Query which entities' geometry overlaps with the bounds.
    #[tracing::instrument(skip_all)]
    fn query_items(&self, left: f32, top: f32, right: f32, bottom: f32) -> Vec<ItemHandle> {
        let mut is: Vec<ItemHandle> = vec![];
        for ih in self
            .bounds_index
            .query(left, top, right, bottom)
            .iter()
            .map(|&i| self.item_mapping[i])
        {
            if let Err(i) = is.binary_search(&ih) {
                is.insert(i, ih);
            }
        }
        is
    }

    fn bounds(&self) -> Rect {
        self.bounds_index
            .bounds()
            .map_or(Rect::default(), |b| Rect {
                x0: b.min_x as f64,
                y0: b.min_y as f64,
                x1: b.max_x as f64,
                y1: b.max_y as f64,
            })
    }
}

/// Compute an index of bounding boxes for shapes.
#[allow(
    clippy::cast_possible_truncation,
    reason = "The loss of range and precision is acceptable."
)]
#[tracing::instrument(skip_all)]
fn compute_bounds_index(lines: &[PathSeg]) -> StaticAABB2DIndex<f32> {
    let mut builder = StaticAABB2DIndexBuilder::<f32>::new(lines.len());
    for shape in lines.iter() {
        let bbox = Shape::bounding_box(&shape);
        builder.add(
            bbox.min_x() as f32,
            bbox.min_y() as f32,
            bbox.max_x() as f32,
            bbox.max_y() as f32,
        );
    }
    builder.build().unwrap()
}

/// Index for culling text items.
struct TextCullIndex {
    bounds_index: StaticAABB2DIndex<f32>,
    item_mapping: Box<[ItemHandle]>,
}

#[allow(
    clippy::cast_possible_truncation,
    reason = "The loss of range and precision is acceptable."
)]
impl TextCullIndex {
    fn new(tv_env: &mut tabulon_vello::Environment, d: &TDDrawing) -> Self {
        let measurements = tv_env.measure_text_items(&d.graphics, &d.render_layer);
        let mut builder = StaticAABB2DIndexBuilder::<f32>::new(measurements.len());
        let mut item_mapping = vec![];

        for (ih, (di, s)) in measurements {
            item_mapping.push(ih);
            let bbox = (Affine::from(di)
                * Rect::from_origin_size(Point::ZERO, s).to_path(DEFAULT_ACCURACY))
            .bounding_box();
            builder.add(
                bbox.min_x() as f32,
                bbox.min_y() as f32,
                bbox.max_x() as f32,
                bbox.max_y() as f32,
            );
        }

        Self {
            bounds_index: builder.build().unwrap(),
            item_mapping: item_mapping.into(),
        }
    }

    /// Query which text layouts overlap with the bounds.
    #[tracing::instrument(skip_all)]
    fn query_items(&self, left: f32, top: f32, right: f32, bottom: f32) -> BTreeSet<ItemHandle> {
        self.bounds_index
            .query(left, top, right, bottom)
            .iter()
            .map(|&l| self.item_mapping[l])
            .collect()
    }
}
