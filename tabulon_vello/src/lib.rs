// Copyright 2025 the Tabulon Authors
// SPDX-License-Identifier: Apache-2.0 OR MIT

//! Vello rendering utilities for Tabulon.

use tabulon::{
    DirectIsometry, GraphicsBag, GraphicsItem, ItemHandle,
    peniko::{
        Color, Fill,
        kurbo::{Affine, Size, Vec2},
    },
    render_layer::RenderLayer,
    shape::{FatPaint, FatShape},
    text::{AttachmentPoint, FatText},
};

use parley::{FontContext, LayoutContext, PositionedLayoutItem};
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

impl Environment {
    /// Add a [`RenderLayer`] to a Vello [`Scene`].
    #[tracing::instrument(skip_all)]
    pub fn add_render_layer_to_scene(
        &mut self,
        scene: &mut Scene,
        graphics: &GraphicsBag,
        render_layer: &RenderLayer,
    ) {
        let Self { font_cx, layout_cx } = self;

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

                    let placement_transform = Affine::from(*insertion)
                        * Affine::translate(-attachment_point.select(layout_size));

                    let FatPaint {
                        fill_paint: Some(fill_paint),
                        ..
                    } = graphics.get_paint(*paint)
                    else {
                        continue;
                    };

                    for line in layout.lines() {
                        for item in line.items() {
                            let PositionedLayoutItem::GlyphRun(glyph_run) = item else {
                                continue;
                            };

                            let mut x = glyph_run.offset();
                            let y = glyph_run.baseline();
                            let run = glyph_run.run();
                            let synthesis = run.synthesis();
                            scene
                                .draw_glyphs(run.font())
                                // TODO: Color will come from styled text.
                                .brush(fill_paint)
                                .hint(false)
                                .transform(transform * placement_transform)
                                .glyph_transform(Some(if let Some(angle) = synthesis.skew() {
                                    Affine::scale(50_f64.recip())
                                        * Affine::skew(angle.to_radians().tan() as f64, 0.0)
                                } else {
                                    Affine::scale(50_f64.recip())
                                }))
                                // Small font sizes are quantized, multiplying by
                                // 50 and then scaling by 1 / 50 at the glyph level
                                // works around this, but it is a hack.
                                .font_size(run.font_size() * 50.0)
                                .normalized_coords(run.normalized_coords())
                                .draw(
                                    Fill::NonZero,
                                    glyph_run.glyphs().map(|g| {
                                        let gx = x + g.x;
                                        let gy = y - g.y;
                                        x += g.advance;
                                        vello::Glyph {
                                            id: g.id as _,
                                            x: gx,
                                            y: gy,
                                        }
                                    }),
                                );
                        }
                    }
                }
            }
        }
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

/// Calculate a top left equivalent insertion point for a layout size and attachment point.
fn rotate_offset(attachment_point: AttachmentPoint, layout_size: Size, angle: f64) -> Vec2 {
    let attachment = attachment_point.select(layout_size);
    let (sin, cos) = angle.sin_cos();
    Vec2 {
        x: attachment.x * cos - attachment.y * sin,
        y: attachment.x * sin + attachment.y * cos,
    }
}
