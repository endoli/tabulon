// Copyright 2025 the Tabulon Authors
// SPDX-License-Identifier: Apache-2.0 OR MIT

//! Convert a DXF (via `tabulon_dxf`) into a simple SVG.
//!
//! This is intentionally a "best-effort" exporter: it emits vector paths and SVG `<text>` elements
//! without attempting full typography fidelity.

use anyhow::{Context, Result};
use joto_constants::length::u64::MICROMETER;
use std::{
    collections::BTreeSet,
    env,
    fs::File,
    io::{self, BufWriter, Write},
    ops::Range,
    path::Path,
};
use tabulon::peniko::Color;
use tabulon::{
    GraphicsBag, GraphicsItem, PaintHandle, peniko::Brush, render_layer::RenderLayer,
    shape::FatShape, text::FatText,
};
use tabulon_dxf::{RestrokePaint, TDDrawing};

#[derive(Default)]
struct ParleyEnv {
    font_cx: parley::FontContext,
    layout_cx: parley::LayoutContext<Option<Color>>,
}

fn usage(argv0: &str) -> ! {
    eprintln!("Usage: {argv0} <input.dxf> [output.svg]");
    std::process::exit(2);
}

fn xml_escape(input: &str) -> String {
    let mut out = String::with_capacity(input.len());
    for ch in input.chars() {
        match ch {
            '&' => out.push_str("&amp;"),
            '<' => out.push_str("&lt;"),
            '>' => out.push_str("&gt;"),
            '"' => out.push_str("&quot;"),
            '\'' => out.push_str("&apos;"),
            _ => out.push(ch),
        }
    }
    out
}

fn svg_matrix(affine: tabulon::peniko::kurbo::Affine) -> String {
    let [a, b, c, d, e, f] = affine.as_coeffs();
    // kurbo uses the same augmented matrix layout as SVG's `matrix(a b c d e f)`.
    format!("matrix({a} {b} {c} {d} {e} {f})")
}

fn svg_path_d(path: &tabulon::peniko::kurbo::BezPath) -> String {
    path.to_svg()
}

fn path_has_draw_ops(path: &tabulon::peniko::kurbo::BezPath) -> bool {
    use tabulon::peniko::kurbo::PathEl;
    path.elements().iter().any(|el| {
        matches!(
            el,
            PathEl::LineTo(_) | PathEl::QuadTo(_, _) | PathEl::CurveTo(_, _, _)
        )
    })
}

fn brush_to_css_color(brush: &Brush) -> Option<String> {
    match brush {
        Brush::Solid(c) => Some(format!("{:x}", c.to_rgba8())),
        _ => None,
    }
}

fn stroke_width_css(paint: PaintHandle, restrokes: &[RestrokePaint]) -> String {
    // The drawing stores stroke weights in iota; `RestrokePaint::weight` is a physical weight.
    // Export as a CSS expression to clamp stroke width independent of transforms.
    let um = restrokes
        .iter()
        .find(|r| r.handle == paint)
        .map(|r| r.weight / MICROMETER)
        .unwrap_or(0);
    format!("min(0.7071px, {um}um)")
}

/// Light adapt paints.
///
/// The ACI palette and drawings using it assume a black background; this adapts colors to have a
/// reasonable degree of contrast for a light SVG background.
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

fn font_size_px(text: &FatText) -> Option<f32> {
    use core::mem::discriminant;
    use parley::StyleProperty;

    text.style
        .inner()
        .get(&discriminant(&StyleProperty::FontSize(0.0)))
        .and_then(|p| match p {
            StyleProperty::FontSize(fs) => Some(*fs),
            _ => None,
        })
}

fn font_stack_css(text: &FatText) -> Option<String> {
    use parley::StyleProperty;

    text.style.inner().values().find_map(|p| match p {
        StyleProperty::FontStack(stack) => Some(match stack {
            parley::style::FontStack::Source(s) => s.as_ref().to_owned(),
            parley::style::FontStack::Single(fam) => fam.to_string(),
            parley::style::FontStack::List(list) => list
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(", "),
        }),
        _ => None,
    })
}

fn build_parley_layout<'a>(
    env: &'a mut ParleyEnv,
    fat_text: &'a FatText,
) -> parley::Layout<Option<Color>> {
    let ParleyEnv { font_cx, layout_cx } = env;
    let text = fat_text.text.as_ref();
    let mut builder = layout_cx.ranged_builder(font_cx, text, 1.0, false);
    for prop in fat_text.style.inner().values() {
        builder.push_default(prop.to_owned());
    }
    let mut layout = builder.build(text);
    layout.break_all_lines(fat_text.max_inline_size);
    layout.align(
        fat_text.max_inline_size,
        fat_text.alignment,
        Default::default(),
    );
    layout
}

fn glyph_run_text_range(glyph_run: &parley::GlyphRun<'_, Option<Color>>) -> Option<Range<usize>> {
    let needle: Vec<parley::Glyph> = glyph_run.glyphs().collect();
    if needle.is_empty() {
        return None;
    }

    let hay: Vec<parley::Glyph> = glyph_run
        .run()
        .visual_clusters()
        .flat_map(|c| c.glyphs())
        .collect();

    let start = hay
        .windows(needle.len())
        .position(|w| w == needle.as_slice())?;
    let end = start + needle.len();

    let mut glyph_cursor = 0_usize;
    let mut out: Option<Range<usize>> = None;
    for cluster in glyph_run.run().visual_clusters() {
        let cluster_glyphs = cluster.glyphs().count();
        let cluster_start = glyph_cursor;
        let cluster_end = glyph_cursor + cluster_glyphs;

        if cluster_end > start && cluster_start < end {
            let tr = cluster.text_range();
            out = Some(match out {
                Some(r) => r.start.min(tr.start)..r.end.max(tr.end),
                None => tr,
            });
        }

        glyph_cursor = cluster_end;
        if glyph_cursor >= end {
            break;
        }
    }

    out
}

fn layout_size(
    fat_text: &FatText,
    layout: &parley::Layout<Option<Color>>,
) -> tabulon::peniko::kurbo::Size {
    tabulon::peniko::kurbo::Size {
        width: fat_text.max_inline_size.unwrap_or(layout.width()) as f64,
        height: layout.height() as f64,
    }
}

fn compute_view_box(env: &mut ParleyEnv, td: &TDDrawing) -> tabulon::peniko::kurbo::Rect {
    use tabulon::peniko::kurbo::{Affine, BezPath, Point, Rect, Shape};
    let mut bounds: Option<Rect> = None;

    for ih in td.render_layer.indices.iter().copied() {
        match td.graphics.get(ih) {
            GraphicsItem::FatShape(FatShape {
                transform, path, ..
            }) => {
                if path.is_empty() || !path_has_draw_ops(path) {
                    continue;
                }
                let affine = td.graphics.get_transform(*transform);
                let mut p: BezPath = (**path).clone();
                p.apply_affine(affine);
                if p.is_empty() || !path_has_draw_ops(&p) {
                    continue;
                }
                let bb = p.bounding_box();
                bounds = Some(bounds.map_or(bb, |b| b.union(bb)));
            }
            GraphicsItem::FatText(t) => {
                if t.text.trim().is_empty() {
                    continue;
                }
                let layout = build_parley_layout(env, t);
                let size = layout_size(t, &layout);
                let placement_transform =
                    Affine::from(t.insertion) * Affine::translate(-t.attachment_point.select(size));

                let outer = td.graphics.get_transform(t.transform);
                let full = outer * placement_transform;

                let corners = [
                    Point::new(0.0, 0.0),
                    Point::new(size.width, 0.0),
                    Point::new(size.width, size.height),
                    Point::new(0.0, size.height),
                ];

                let mut bb = Rect::default();
                for (idx, p) in corners.into_iter().enumerate() {
                    let p = full * p;
                    bb = if idx == 0 {
                        Rect::from_points(p, p)
                    } else {
                        bb.union_pt(p)
                    };
                }
                bounds = Some(bounds.map_or(bb, |b| b.union(bb)));
            }
        }
    }

    let mut rect = bounds.unwrap_or_else(|| Rect::new(0.0, 0.0, 1.0, 1.0));
    // Guard against degenerate viewBoxes.
    if rect.width() == 0.0 {
        rect.x1 = rect.x0 + 1.0;
    }
    if rect.height() == 0.0 {
        rect.y1 = rect.y0 + 1.0;
    }
    let pad = (rect.width().max(rect.height()) * 0.01).max(1.0);
    rect.inflate(pad, pad)
}

fn write_svg<W: Write>(mut w: W, env: &mut ParleyEnv, td: &TDDrawing) -> Result<()> {
    use tabulon::peniko::kurbo::Rect;

    let Rect { x0, y0, x1, y1 } = compute_view_box(env, td);
    let vb_w = x1 - x0;
    let vb_h = y1 - y0;

    writeln!(
        w,
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"{x0} {y0} {vb_w} {vb_h}\">"
    )?;
    // Apply non-scaling stroke without repeating the attribute on each path element.
    writeln!(
        w,
        "<style>path{{vector-effect:non-scaling-stroke;}}</style>"
    )?;

    for ih in td.render_layer.indices.iter().copied() {
        match td.graphics.get(ih) {
            GraphicsItem::FatShape(shape) => write_shape(&mut w, td, shape)?,
            GraphicsItem::FatText(text) => write_text(&mut w, env, td, text)?,
        }
    }

    writeln!(w, "</svg>")?;
    Ok(())
}

fn write_shape<W: Write>(w: &mut W, td: &TDDrawing, shape: &FatShape) -> Result<()> {
    if shape.path.is_empty() || !path_has_draw_ops(&shape.path) {
        return Ok(());
    }
    let paint = td.graphics.get_paint(shape.paint);
    let fill = paint.fill_paint.as_ref().and_then(brush_to_css_color);
    let stroke = paint.stroke_paint.as_ref().and_then(brush_to_css_color);

    let d = svg_path_d(&shape.path);
    let affine = td.graphics.get_transform(shape.transform);
    let transform_attr =
        (affine != tabulon::peniko::kurbo::Affine::IDENTITY).then(|| svg_matrix(affine));

    write!(w, "<path d=\"{}\"", d)?;
    if let Some(t) = transform_attr {
        write!(w, " transform=\"{}\"", t)?;
    }

    match &stroke {
        Some(c) => {
            let sw = stroke_width_css(shape.paint, &td.restroke_paints);
            write!(w, " stroke=\"{c}\" style=\"stroke-width: {sw};\"")?;
        }
        None => {
            write!(w, " stroke=\"none\"")?;
        }
    }

    match &fill {
        Some(c) => write!(w, " fill=\"{c}\"")?,
        None => write!(w, " fill=\"none\"")?,
    }

    writeln!(w, "/>")?;
    Ok(())
}

fn write_text<W: Write>(
    w: &mut W,
    env: &mut ParleyEnv,
    td: &TDDrawing,
    fat_text: &FatText,
) -> Result<()> {
    fn push_fmt(dst: &mut String, args: std::fmt::Arguments<'_>) {
        use std::fmt::Write as _;
        let _ = dst.write_fmt(args);
    }

    if fat_text.text.trim().is_empty() {
        return Ok(());
    }

    let paint = td.graphics.get_paint(fat_text.paint);
    let fill = paint
        .fill_paint
        .as_ref()
        .and_then(brush_to_css_color)
        .unwrap_or_else(|| "#000000".to_owned());

    let layout = build_parley_layout(env, fat_text);
    let size = layout_size(fat_text, &layout);
    let placement_transform = tabulon::peniko::kurbo::Affine::from(fat_text.insertion)
        * tabulon::peniko::kurbo::Affine::translate(-fat_text.attachment_point.select(size));

    let outer = td.graphics.get_transform(fat_text.transform);
    let full = outer * placement_transform;
    let has_transform = full != tabulon::peniko::kurbo::Affine::IDENTITY;

    let (text_anchor, anchor_x) = match fat_text.alignment {
        parley::Alignment::Center => ("middle", size.width / 2.0),
        parley::Alignment::End | parley::Alignment::Right => ("end", size.width),
        parley::Alignment::Start | parley::Alignment::Left | parley::Alignment::Justify => {
            ("start", 0.0)
        }
    };

    let full_text = fat_text.text.as_ref();
    if layout.lines().count() == 1 {
        let Some(line) = layout.lines().next() else {
            return Ok(());
        };
        let range = line.text_range();
        let Some(slice) = full_text.get(range) else {
            return Ok(());
        };
        let slice = slice.trim_matches(['\n', '\r']);
        if slice.is_empty() {
            return Ok(());
        }

        if has_transform {
            writeln!(w, "<g transform=\"{}\">", svg_matrix(full))?;
        }

        let line_y = line.metrics().baseline;
        write!(
            w,
            "<text x=\"{anchor_x}\" y=\"{line_y}\" fill=\"{fill}\" text-anchor=\"{text_anchor}\" xml:space=\"preserve\""
        )?;

        if let Some(fs) = font_size_px(fat_text) {
            write!(w, " font-size=\"{fs}\"")?;
        } else {
            let run_fs = line.items().find_map(|item| match item {
                parley::PositionedLayoutItem::GlyphRun(gr) => Some(gr.run().font_size()),
                parley::PositionedLayoutItem::InlineBox(_) => None,
            });
            if let Some(run_fs) = run_fs {
                write!(w, " font-size=\"{run_fs}\"")?;
            }
        }
        if let Some(stack) = font_stack_css(fat_text) {
            write!(w, " font-family=\"{}\"", xml_escape(&stack))?;
        }

        write!(w, ">")?;
        write!(w, "{}", xml_escape(slice))?;
        writeln!(w, "</text>")?;

        if has_transform {
            writeln!(w, "</g>")?;
        }

        return Ok(());
    }

    let mut body = String::new();
    let mut emitted_any = false;

    for line in layout.lines() {
        let mut emitted_line = false;
        let mut is_first_span_on_line = true;
        let line_y = line.metrics().baseline;

        for item in line.items() {
            let parley::PositionedLayoutItem::GlyphRun(glyph_run) = item else {
                continue;
            };

            let range =
                glyph_run_text_range(&glyph_run).unwrap_or_else(|| glyph_run.run().text_range());
            let Some(slice) = full_text.get(range) else {
                continue;
            };
            let slice = slice.trim_matches(['\n', '\r']);
            if slice.is_empty() {
                continue;
            }

            // Parley determines line breaking and line height, but SVG's `text-anchor` controls
            // horizontal alignment within a line.
            let run_fs = glyph_run.run().font_size();
            if is_first_span_on_line {
                push_fmt(
                    &mut body,
                    format_args!(
                        "<tspan x=\"{anchor_x}\" y=\"{line_y}\" xml:space=\"preserve\" font-size=\"{run_fs}\">"
                    ),
                );
                is_first_span_on_line = false;
            } else {
                push_fmt(
                    &mut body,
                    format_args!("<tspan xml:space=\"preserve\" font-size=\"{run_fs}\">"),
                );
            }
            body.push_str(&xml_escape(slice));
            body.push_str("</tspan>");

            emitted_any = true;
            emitted_line = true;
        }

        if !emitted_line {
            // Fall back to emitting one tspan per Parley line using the line's computed text range.
            let range = line.text_range();
            let Some(slice) = full_text.get(range) else {
                continue;
            };
            let slice = slice.trim_matches(['\n', '\r']);
            if slice.is_empty() {
                continue;
            }

            push_fmt(
                &mut body,
                format_args!("<tspan x=\"{anchor_x}\" y=\"{line_y}\" xml:space=\"preserve\">"),
            );
            body.push_str(&xml_escape(slice));
            body.push_str("</tspan>");
            emitted_any = true;
        }
    }

    if !emitted_any {
        return Ok(());
    }

    if has_transform {
        writeln!(w, "<g transform=\"{}\">", svg_matrix(full))?;
    }

    write!(w, "<text fill=\"{fill}\" text-anchor=\"{text_anchor}\"")?;

    if let Some(fs) = font_size_px(fat_text) {
        write!(w, " font-size=\"{fs}\"")?;
    }
    if let Some(stack) = font_stack_css(fat_text) {
        write!(w, " font-family=\"{}\"", xml_escape(&stack))?;
    }

    write!(w, ">")?;
    write!(w, "{body}")?;
    writeln!(w, "</text>")?;

    if has_transform {
        writeln!(w, "</g>")?;
    }

    Ok(())
}

fn main() -> Result<()> {
    let mut args = env::args();
    let argv0 = args.next().unwrap_or_else(|| "dxf_to_svg".to_owned());
    let input = args.next().unwrap_or_else(|| usage(&argv0));
    let output = args.next();

    if args.next().is_some() {
        usage(&argv0);
    }

    let mut td = tabulon_dxf::load_file_default_layers(&input)
        .with_context(|| format!("loading DXF from {input:?}"))?;

    light_adapt_paints(&mut td.graphics, &td.render_layer);

    let mut env = ParleyEnv::default();

    match output {
        Some(p) => {
            let file = File::create(Path::new(&p)).with_context(|| format!("creating {p:?}"))?;
            let w = BufWriter::new(file);
            write_svg(w, &mut env, &td)?;
        }
        None => {
            let stdout = io::stdout();
            let w = BufWriter::new(stdout.lock());
            write_svg(w, &mut env, &td)?;
        }
    }

    Ok(())
}
