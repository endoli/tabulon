// Copyright 2024 the Vello Authors
// Copyright 2025 the Tabulon Authors
// SPDX-License-Identifier: Apache-2.0 OR MIT

//! Simple example.

use anyhow::Result;
use std::num::NonZeroUsize;
use std::sync::Arc;
use vello::kurbo::{Circle, DEFAULT_ACCURACY, Ellipse, Line, RoundedRect, Shape, Stroke};
use vello::peniko::Color;
use vello::peniko::color::palette;
use vello::util::{RenderContext, RenderSurface};
use vello::{AaConfig, Renderer, RendererOptions, Scene};
use winit::application::ApplicationHandler;
use winit::dpi::LogicalSize;
use winit::event::WindowEvent;
use winit::event_loop::{ActiveEventLoop, EventLoop};
use winit::window::Window;

use vello::wgpu;

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

struct SimpleVelloApp<'s> {
    // The vello RenderContext which is a global context that lasts for the
    // lifetime of the application
    context: RenderContext,

    // An array of renderers, one per wgpu device
    renderers: Vec<Option<Renderer>>,

    // State for our example where we store the winit Window and the wgpu Surface
    state: RenderState<'s>,

    // A vello Scene which is a data structure which allows one to build up a
    // description a scene to be drawn (with paths, fills, images, text, etc)
    // which is then passed to a renderer for rendering
    scene: Scene,

    /// Tabulon Vello environment.
    tv_environment: tabulon_vello::Environment,
}

impl ApplicationHandler for SimpleVelloApp<'_> {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        let RenderState::Suspended(cached_window) = &mut self.state else {
            return;
        };

        // Get the winit window cached in a previous Suspended event or else create a new window
        let window = cached_window
            .take()
            .unwrap_or_else(|| create_winit_window(event_loop));

        // Create a vello Surface
        let size = window.inner_size();
        let surface_future = self.context.create_surface(
            window.clone(),
            size.width,
            size.height,
            wgpu::PresentMode::AutoVsync,
        );
        let surface = pollster::block_on(surface_future).expect("Error creating surface");

        // Create a vello Renderer for the surface (using its device id)
        self.renderers
            .resize_with(self.context.devices.len(), || None);
        self.renderers[surface.dev_id]
            .get_or_insert_with(|| create_vello_renderer(&self.context, &surface));

        // Save the Window and Surface to a state variable
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

    fn window_event(
        &mut self,
        event_loop: &ActiveEventLoop,
        window_id: winit::window::WindowId,
        event: WindowEvent,
    ) {
        let surface = match &mut self.state {
            RenderState::Active { surface, window } if window.id() == window_id => surface,
            _ => return,
        };

        match event {
            WindowEvent::CloseRequested => event_loop.exit(),

            WindowEvent::Resized(size) => {
                self.context
                    .resize_surface(surface, size.width, size.height);
            }

            WindowEvent::RedrawRequested => {
                // Empty the scene of objects to draw. You could create a new Scene each time, but in this case
                // the same Scene is reused so that the underlying memory allocation can also be reused.
                self.scene.reset();

                add_shapes_to_scene(&mut self.tv_environment, &mut self.scene);

                let wgpu::SurfaceConfiguration { width, height, .. } = surface.config;

                let device_handle = &self.context.devices[surface.dev_id];

                let surface_texture = surface
                    .surface
                    .get_current_texture()
                    .expect("failed to get surface texture");

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
                            base_color: palette::css::BLACK, // Background color
                            width,
                            height,
                            antialiasing_method: AaConfig::Msaa16,
                        },
                    )
                    .expect("failed to render to surface");

                let mut encoder =
                    device_handle
                        .device
                        .create_command_encoder(&wgpu::CommandEncoderDescriptor {
                            label: Some("Surface Blit"),
                        });
                surface.blitter.copy(
                    &device_handle.device,
                    &mut encoder,
                    &surface.target_view,
                    &surface_texture
                        .texture
                        .create_view(&wgpu::TextureViewDescriptor::default()),
                );
                device_handle.queue.submit([encoder.finish()]);
                // Queue the texture to be presented on the surface
                surface_texture.present();

                let _ = device_handle.device.poll(wgpu::PollType::Poll);
            }
            _ => {}
        }
    }
}

fn main() -> Result<()> {
    let mut app = SimpleVelloApp {
        context: RenderContext::new(),
        renderers: vec![],
        state: RenderState::Suspended(None),
        scene: Scene::new(),
        tv_environment: Default::default(),
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
        .with_inner_size(LogicalSize::new(1044, 800))
        .with_resizable(true)
        .with_title("Vello Shapes");
    Arc::new(event_loop.create_window(attr).unwrap())
}

/// Helper function that creates a vello `Renderer` for a given `RenderContext` and `RenderSurface`
fn create_vello_renderer(render_cx: &RenderContext, surface: &RenderSurface<'_>) -> Renderer {
    Renderer::new(
        &render_cx.devices[surface.dev_id].device,
        RendererOptions {
            use_cpu: false,
            antialiasing_support: vello::AaSupport::all(),
            num_init_threads: NonZeroUsize::new(1),
            pipeline_cache: None,
        },
    )
    .expect("Couldn't create renderer")
}

/// Add shapes to a vello scene. This does not actually render the shapes, but adds them
/// to the Scene data structure which represents a set of objects to draw.
fn add_shapes_to_scene(tv_environment: &mut tabulon_vello::Environment, scene: &mut Scene) {
    use tabulon::shape::{FatPaint, FatShape};
    use tabulon::{graphics_bag::GraphicsBag, render_layer::RenderLayer};

    let mut rl = RenderLayer::default();
    let mut gb = GraphicsBag::default();

    // Draw an outlined rectangle
    let paint = gb.register_paint(FatPaint {
        stroke: Stroke::new(6.0),
        stroke_paint: Some(Color::new([0.9804, 0.702, 0.5294, 1.]).into()),
        fill_paint: None,
    });
    rl.push_with_bag(
        &mut gb,
        FatShape {
            transform: Default::default(),
            paint,
            path: Arc::from(
                RoundedRect::new(10.0, 10.0, 240.0, 240.0, 20.0).to_path(DEFAULT_ACCURACY),
            ),
        },
    );

    // Draw a filled circle
    let paint = gb.register_paint(FatPaint {
        stroke: Default::default(),
        stroke_paint: None,
        fill_paint: Some(Color::new([0.9529, 0.5451, 0.6588, 1.]).into()),
    });
    rl.push_with_bag(
        &mut gb,
        FatShape {
            transform: Default::default(),
            paint,
            path: Arc::from(Circle::new((420.0, 200.0), 120.0).to_path(DEFAULT_ACCURACY)),
        },
    );

    // Draw a filled ellipse
    let paint = gb.register_paint(FatPaint {
        stroke: Default::default(),
        stroke_paint: None,
        fill_paint: Some(Color::new([0.7961, 0.651, 0.9686, 1.]).into()),
    });
    rl.push_with_bag(
        &mut gb,
        FatShape {
            transform: Default::default(),
            paint,
            path: Arc::from(
                Ellipse::new((250.0, 420.0), (100.0, 160.0), -90.0).to_path(DEFAULT_ACCURACY),
            ),
        },
    );

    // Draw a straight line
    let paint = gb.register_paint(FatPaint {
        stroke: Stroke::new(6.0),
        stroke_paint: Some(Color::new([0.5373, 0.7059, 0.9804, 1.]).into()),
        fill_paint: None,
    });
    rl.push_with_bag(
        &mut gb,
        FatShape {
            transform: Default::default(),
            paint,
            path: Arc::from(Line::new((260.0, 20.0), (620.0, 100.0)).to_path(DEFAULT_ACCURACY)),
        },
    );

    tv_environment.add_render_layer_to_scene(scene, &gb, &rl);
}
