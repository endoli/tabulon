// Copyright 2025 the Tabulon Authors
// SPDX-License-Identifier: Apache-2.0 OR MIT

//! Vello rendering utilities for Tabulon.

use tabulon::{
    DirectIsometry, GraphicsBag, GraphicsItem, ItemHandle,
    peniko::{
        Brush, Color, Fill,
        kurbo::{Affine, Size, Vec2},
    },
    render_layer::RenderLayer,
    shape::{FatPaint, FatShape},
    text::{AttachmentPoint, FatText},
};

use parley::{FontContext, Layout, LayoutContext, PositionedLayoutItem};
use vello::{Scene, peniko::Fill::NonZero};

extern crate alloc;
use alloc::collections::BTreeMap;

/// Expensive state for rendering.
#[derive(Default)]
#[allow(
    missing_debug_implementations,
    reason = "Not useful, and members don't implement Debug."
)]
pub struct Environment {
    /// Font context.
    ///
    /// This contains a font collection that is expensive to reproduce.
    pub(crate) font_cx: FontContext,
    /// Layout context.
    pub(crate) layout_cx: LayoutContext<Option<Color>>,
}

/// Convenience type for layout caches.
pub type LayoutCache = BTreeMap<ItemHandle, Layout<Option<Color>>>;

impl Environment {
    /// Add a [`RenderLayer`] to a Vello [`Scene`].
    #[tracing::instrument(skip_all)]
    pub fn add_render_layer_to_scene(
        &mut self,
        scene: &mut Scene,
        graphics: &GraphicsBag,
        render_layer: &RenderLayer,
        layout_cache: Option<&LayoutCache>,
    ) {
        let font_cx = &mut self.font_cx;
        let layout_cx = &mut self.layout_cx;

        for idx in &render_layer.indices {
            match graphics.get(*idx) {
                GraphicsItem::FatShape(FatShape {
                    paint,
                    transform,
                    path,
                }) => {
                    let transform = graphics.get_transform(*transform);
                    let FatPaint {
                        stroke,
                        stroke_paint,
                        fill_paint,
                    } = graphics.get_paint(*paint);

                    if let Some(fill_paint) = fill_paint {
                        scene.fill(NonZero, transform, fill_paint, None, path.as_ref());
                    }
                    if let Some(stroke_paint) = stroke_paint {
                        scene.stroke(stroke, transform, stroke_paint, None, path.as_ref());
                    }
                }
                GraphicsItem::FatText(FatText {
                    transform,
                    paint,
                    text,
                    style,
                    max_inline_size,
                    alignment,
                    insertion,
                    attachment_point,
                }) => {
                    let transform = graphics.get_transform(*transform);

                    let FatPaint {
                        fill_paint: Some(fill_paint),
                        ..
                    } = graphics.get_paint(*paint)
                    else {
                        continue;
                    };

                    if let Some(l) = layout_cache.and_then(|cache| cache.get(idx)) {
                        let layout_size = Size {
                            width: max_inline_size.unwrap_or(l.width()) as f64,
                            height: l.height() as f64,
                        };

                        let placement_transform = Affine::from(*insertion)
                            * Affine::translate(-attachment_point.select(layout_size));

                        draw_layout(scene, transform * placement_transform, l, fill_paint);
                    } else {
                        let mut builder = layout_cx.ranged_builder(font_cx, text, 1.0, false);
                        for prop in style.inner().values() {
                            builder.push_default(prop.to_owned());
                        }
                        let mut l = builder.build(text);
                        l.break_all_lines(*max_inline_size);
                        l.align(*max_inline_size, *alignment, Default::default());

                        let layout_size = Size {
                            width: max_inline_size.unwrap_or(l.width()) as f64,
                            height: l.height() as f64,
                        };

                        let placement_transform = Affine::from(*insertion)
                            * Affine::translate(-attachment_point.select(layout_size));

                        draw_layout(scene, transform * placement_transform, &l, fill_paint);
                    };
                }
            }
        }
    }

    /// Compute layouts and measures for text items in a [`RenderLayer`].
    #[tracing::instrument(skip_all)]
    pub fn compute_text_layouts_and_measures(
        &mut self,
        graphics: &GraphicsBag,
        render_layer: &RenderLayer,
    ) -> (LayoutCache, BTreeMap<ItemHandle, (DirectIsometry, Size)>) {
        let mut layouts = BTreeMap::new();
        let mut measures = BTreeMap::new();

        let font_cx = &mut self.font_cx;
        let layout_cx = &mut self.layout_cx;

        for idx in &render_layer.indices {
            let GraphicsItem::FatText(FatText {
                text,
                style,
                max_inline_size,
                alignment,
                insertion,
                attachment_point,
                ..
            }) = graphics.get(*idx)
            else {
                continue;
            };

            let mut builder = layout_cx.ranged_builder(font_cx, text, 1.0, false);
            for prop in style.inner().values() {
                builder.push_default(prop.to_owned());
            }
            let mut layout = builder.build(text);
            layout.break_all_lines(*max_inline_size);
            layout.align(*max_inline_size, *alignment, Default::default());

            let layout_size = Size {
                width: max_inline_size.unwrap_or(layout.width()) as f64,
                height: layout.height() as f64,
            };

            let rotated_offset = rotate_offset(*attachment_point, layout_size, insertion.angle);

            measures.insert(
                *idx,
                (
                    DirectIsometry {
                        displacement: insertion.displacement - rotated_offset,
                        ..*insertion
                    },
                    layout_size,
                ),
            );

            layouts.insert(*idx, layout);
        }

        (layouts, measures)
    }

    /// Measure text items in a [`RenderLayer`].
    #[tracing::instrument(skip_all)]
    pub fn measure_text_items(
        &mut self,
        graphics: &GraphicsBag,
        render_layer: &RenderLayer,
    ) -> BTreeMap<ItemHandle, (DirectIsometry, Size)> {
        let Self { font_cx, layout_cx } = self;
        let mut out = BTreeMap::new();

        for idx in &render_layer.indices {
            let GraphicsItem::FatText(FatText {
                text,
                style,
                max_inline_size,
                alignment,
                insertion,
                attachment_point,
                ..
            }) = graphics.get(*idx)
            else {
                continue;
            };

            let mut builder = layout_cx.ranged_builder(font_cx, text, 1.0, false);
            for prop in style.inner().values() {
                builder.push_default(prop.to_owned());
            }
            let mut layout = builder.build(text);
            layout.break_all_lines(*max_inline_size);
            layout.align(*max_inline_size, *alignment, Default::default());

            let layout_size = Size {
                width: max_inline_size.unwrap_or(layout.width()) as f64,
                height: layout.height() as f64,
            };

            let rotated_offset = rotate_offset(*attachment_point, layout_size, insertion.angle);

            out.insert(
                *idx,
                (
                    DirectIsometry {
                        displacement: insertion.displacement - rotated_offset,
                        ..*insertion
                    },
                    layout_size,
                ),
            );
        }

        out
    }
}

/// Draw a layout to a scene.
fn draw_layout(
    scene: &mut Scene,
    transform: Affine,
    layout: &Layout<Option<Color>>,
    fill_paint: &Brush,
) {
    for line in layout.lines() {
        for item in line.items() {
            let PositionedLayoutItem::GlyphRun(glyph_run) = item else {
                continue;
            };

            let mut x = glyph_run.offset();
            let y = glyph_run.baseline();
            let run = glyph_run.run();

            // Vello has a hard time drawing glyphs either very large or very
            // small, so we render at 1000 units regardless, and then transform.
            let fudge = run.font_size() as f64 / 1000.0;

            let synthesis = run.synthesis();
            scene
                .draw_glyphs(run.font())
                // TODO: Color will come from styled text.
                .brush(fill_paint)
                .hint(false)
                .transform(transform)
                .glyph_transform(Some(if let Some(angle) = synthesis.skew() {
                    Affine::scale(fudge) * Affine::skew(angle.to_radians().tan() as f64, 0.0)
                } else {
                    Affine::scale(fudge)
                }))
                // Small font sizes are quantized, multiplying by
                // 50 and then scaling by 1 / 50 at the glyph level
                // works around this, but it is a hack.
                .font_size(1000_f32)
                .normalized_coords(run.normalized_coords())
                .draw(
                    Fill::NonZero,
                    glyph_run.glyphs().map(|g| {
                        let gx = x + g.x;
                        let gy = y - g.y;
                        x += g.advance;
                        vello::Glyph {
                            id: g.id,
                            x: gx,
                            y: gy,
                        }
                    }),
                );
        }
    }
}

/// Calculate a top left equivalent insertion point for a layout size and attachment point.
fn rotate_offset(attachment_point: AttachmentPoint, layout_size: Size, angle: f64) -> Vec2 {
    let attachment = attachment_point.select(layout_size);
    let (sin, cos) = angle.sin_cos();
    Vec2 {
        x: attachment.x * cos - attachment.y * sin,
        y: attachment.x * sin + attachment.y * cos,
    }
}
