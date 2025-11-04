// Copyright 2025 the Tabulon Authors
// SPDX-License-Identifier: Apache-2.0 OR MIT

//! DXF loader for Tabulon

pub use dxf;
use dxf::{
    Drawing, DxfResult,
    entities::EntityType,
    enums::{HorizontalTextJustification, VerticalTextJustification},
};

use tabulon::{
    DirectIsometry, GraphicsBag, GraphicsItem, ItemHandle, PaintHandle,
    peniko::{
        Color,
        kurbo::{
            Affine, Arc, BezPath, Circle, DEFAULT_ACCURACY, Line, PathEl, Point, Shape, Stroke,
            Vec2,
        },
    },
    render_layer::RenderLayer,
    shape::{FatPaint, FatShape},
    text::{AttachmentPoint, FatText},
};

use joto_constants::u64::MICROMETER;
use parley::{Alignment, LineHeight, StyleSet};

extern crate alloc;
use alloc::{
    collections::{btree_map::BTreeMap, btree_set::BTreeSet},
    sync,
};

#[cfg(feature = "std")]
use std::path::Path;

use core::{cmp::Ordering, mem::discriminant, num::NonZeroU64};

mod aci_palette;
use aci_palette::ACI;

/// A valid handle for an [`Entity`](dxf::entities::Entity) present in the drawing.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct EntityHandle(pub(crate) NonZeroU64);

/// A valid handle for a [`Layer`](dxf::tables::Layer) present in the drawing.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct LayerHandle(pub(crate) NonZeroU64);

#[derive(Debug, Clone)]
struct TextItem {
    text: sync::Arc<str>,
    style: StyleSet<Option<Color>>,
    alignment: Alignment,
    insertion: DirectIsometry,
    max_inline_size: Option<f32>,
    attachment_point: AttachmentPoint,
}

/// An chunk of things drawn in order.
#[derive(Debug, Clone, Default)]
enum BlockChunk {
    /// Something not yet supported.
    #[default]
    Unsupported,
    /// Path with a particular color and weight.
    Path(i16, i16, BezPath),
    /// Text item. DXF color enum and [`TextItem`].
    Text(i16, TextItem),
}

/// Convert an entity to a [`BlockChunk`].
fn chunk_from_entity(
    e: &dxf::entities::Entity,
    styles: &BTreeMap<&str, StyleSet<Option<Color>>>,
) -> BlockChunk {
    let ce = recover_color_enum(&e.common.color);
    match e.specific {
        #[allow(clippy::cast_possible_truncation, reason = "It doesn't matter")]
        EntityType::MText(ref mt) => {
            // FIXME: currently only support viewing from +Z.
            if mt.extrusion_direction.z != 1.0 {
                return BlockChunk::Unsupported;
            }

            // TODO: Parse MTEXT encoded characters to Unicode equivalents.
            // TODO: Set up background fills.
            // TODO: Handle inline style changes?
            // TODO: Handle columns.
            // TODO: Handle paragraph styles.
            // TODO: Handle rotation.
            let mut nt = mt.text.clone();
            for ext in mt.extended_text.iter() {
                nt.push_str(ext);
            }

            // TODO: Implement a shared parser for scanning formatting codes into styled text
            //       and doing unicode substitution for special character codes.
            let nt = nt
                .replace("%%c", "∅")
                .replace("%%d", "°")
                .replace("%%p", "±")
                .replace("%%C", "∅")
                .replace("%%D", "°")
                .replace("%%P", "±")
                .replace("%%%", "%")
                // TODO: Implement start/stop underline with styled text.
                .replace("\\L", "")
                .replace("\\l", "")
                // TODO: Implement start/stop overline with styled text.
                .replace("\\O", "")
                .replace("\\o", "")
                // TODO: Implement start/stop strikethrough with styled text.
                .replace("\\S", "")
                .replace("\\s", "")
                .replace("\\P", "\n")
                .replace("\\A1;", "")
                .replace("\\A0;", "")
                .replace("\\pxqc;", "")
                .replace("\\pxql;", "")
                .replace("\\pxqr;", "");

            let x_angle = Vec2 {
                x: mt.x_axis_direction.x,
                y: -mt.x_axis_direction.y,
            }
            .atan2();

            let attachment_point = dxf_attachment_point_to_tabulon(mt.attachment_point);

            // In DXF, the text alignment is also decided by the attachment point.
            let alignment = {
                use Alignment::*;
                use AttachmentPoint::*;
                match attachment_point {
                    TopCenter | MiddleCenter | BottomCenter => Center,
                    TopLeft | MiddleLeft | BottomLeft => Left,
                    TopRight | MiddleRight | BottomRight => Right,
                }
            };

            let max_inline_size = if alignment == Alignment::Center {
                None
            } else {
                match mt.column_type {
                    0 => (mt.reference_rectangle_width != 0.0)
                        .then_some(mt.reference_rectangle_width as f32),
                    1 => (mt.column_width != 0.0).then_some(mt.column_width as f32),
                    _ => None,
                }
            };

            BlockChunk::Text(
                ce,
                TextItem {
                    text: nt.into(),
                    // TODO: Map more styling information from the MText
                    style: styles.get(mt.text_style_name.as_str()).map_or_else(
                        || StyleSet::new(mt.initial_text_height as f32),
                        |s| {
                            if style_size_is_zero(s) {
                                let mut news = s.clone();
                                news.insert(StyleProperty::FontSize(mt.initial_text_height as f32));
                                news
                            } else {
                                s.clone()
                            }
                        },
                    ),
                    alignment,
                    insertion: DirectIsometry::new(
                        // As far as I'm aware, x_axis_direction and rotation are exclusive.
                        -mt.rotation_angle.to_radians() + x_angle,
                        point_from_dxf_point(&mt.insertion_point).to_vec2(),
                    ),
                    max_inline_size,
                    attachment_point,
                },
            )
        }
        #[allow(clippy::cast_possible_truncation, reason = "It doesn't matter")]
        EntityType::Text(ref t) => {
            // FIXME: currently only support viewing from +Z.
            if t.normal.z != 1.0 {
                return BlockChunk::Unsupported;
            }

            // TODO: Handle second_alignment_point etc?
            // TODO: Handle relative_x_scale_factor.

            // TODO: Implement a shared parser for scanning formatting codes into styled text
            //       and doing unicode substitution for special character codes.
            let text = t
                .value
                .replace("%%c", "∅")
                .replace("%%d", "°")
                .replace("%%p", "±")
                .replace("%%C", "∅")
                .replace("%%D", "°")
                .replace("%%P", "±")
                .replace("%%%", "%")
                // TODO: implement toggle underline with styled text.
                .replace("%%u", "")
                // TODO: implement toggle overline with styled text.
                .replace("%%o", "");

            let attachment_point = {
                use HorizontalTextJustification as H;
                use VerticalTextJustification as V;
                match (
                    t.horizontal_text_justification,
                    t.vertical_text_justification,
                ) {
                    (H::Left, V::Top) => AttachmentPoint::TopLeft,
                    (H::Center | H::Middle, V::Top) => AttachmentPoint::TopCenter,
                    (H::Right, V::Top) => AttachmentPoint::TopRight,
                    (H::Left, V::Middle) => AttachmentPoint::MiddleLeft,
                    (H::Center | H::Middle, V::Middle) => AttachmentPoint::MiddleCenter,
                    (H::Right, V::Middle) => AttachmentPoint::MiddleRight,
                    (H::Left, V::Bottom) => AttachmentPoint::BottomLeft,
                    (H::Center | H::Middle, V::Bottom) => AttachmentPoint::BottomCenter,
                    (H::Right, V::Bottom) => AttachmentPoint::BottomRight,
                    (H::Left, V::Baseline) => AttachmentPoint::BottomLeft,
                    (H::Center | H::Middle, V::Baseline) => AttachmentPoint::BottomCenter,
                    (H::Right, V::Baseline) => AttachmentPoint::BottomRight,
                    // These need further attention.
                    (H::Aligned | H::Fit, V::Top) => AttachmentPoint::TopLeft,
                    (H::Aligned | H::Fit, V::Middle) => AttachmentPoint::MiddleLeft,
                    (H::Aligned | H::Fit, V::Bottom | V::Baseline) => AttachmentPoint::BottomLeft,
                }
            };

            let alignment = {
                use HorizontalTextJustification::*;
                match t.horizontal_text_justification {
                    Left => Alignment::Left,
                    Right => Alignment::Right,
                    Center | Middle => Alignment::Center,
                    // These need further attention.
                    Aligned | Fit => Alignment::Left,
                }
            };

            let angle = {
                use HorizontalTextJustification as H;
                match t.horizontal_text_justification {
                    H::Aligned | H::Fit => (point_from_dxf_point(&t.second_alignment_point)
                        .to_vec2()
                        - point_from_dxf_point(&t.location).to_vec2())
                    .angle(),
                    _ => -t.rotation.to_radians(),
                }
            };

            let displacement = {
                use HorizontalTextJustification as H;
                match t.horizontal_text_justification {
                    H::Center => point_from_dxf_point(&t.second_alignment_point).to_vec2(),
                    _ => point_from_dxf_point(&t.location).to_vec2(),
                }
            };

            BlockChunk::Text(
                ce,
                TextItem {
                    text: text.into(),
                    style: styles.get(t.text_style_name.as_str()).map_or_else(
                        || StyleSet::new(t.text_height as f32),
                        |s| {
                            let mut sized = if style_size_is_zero(s) {
                                let mut news = s.clone();
                                news.insert(StyleProperty::FontSize(t.text_height as f32));
                                news
                            } else {
                                s.clone()
                            };
                            if t.oblique_angle != 0.0 {
                                sized.insert(StyleProperty::FontStyle(FontStyle::Oblique(Some(
                                    t.oblique_angle as f32,
                                ))));
                            }
                            sized
                        },
                    ),
                    alignment,
                    insertion: DirectIsometry {
                        angle,
                        displacement,
                    },
                    max_inline_size: None,
                    attachment_point,
                },
            )
        }
        _ => {
            if let Some(p) = path_from_entity(e) {
                BlockChunk::Path(
                    e.common.lineweight_enum_value,
                    recover_color_enum(&e.common.color),
                    p,
                )
            } else {
                BlockChunk::Unsupported
            }
        }
    }
}

/// Convert an entity to a [`BezPath`].
#[tracing::instrument(skip_all)]
pub fn path_from_entity(e: &dxf::entities::Entity) -> Option<BezPath> {
    match e.specific {
        EntityType::Arc(ref a) => {
            // FIXME: currently only support viewing from +Z.
            if a.normal.z != 1.0 {
                return None;
            }

            let dxf::entities::Arc {
                center,
                radius,
                start_angle,
                end_angle,
                ..
            } = a.clone();
            Some(
                Arc {
                    center: point_from_dxf_point(&center),
                    radii: Vec2 {
                        x: radius,
                        y: radius,
                    },
                    // DXF is y-up, so these are originally counterclockwise.
                    start_angle: -start_angle.to_radians(),
                    sweep_angle: -(end_angle - start_angle).rem_euclid(360.0).to_radians(),
                    x_rotation: 0.0,
                }
                .to_path(DEFAULT_ACCURACY),
            )
        }
        EntityType::Line(ref line) => {
            // FIXME: currently only support viewing from +Z.
            if line.extrusion_direction.z != 1.0 {
                return None;
            }

            let mut l = BezPath::new();
            l.move_to(point_from_dxf_point(&line.p1));
            l.line_to(point_from_dxf_point(&line.p2));
            Some(l)
        }
        EntityType::Circle(ref circle) => {
            // FIXME: currently only support viewing from +Z.
            if circle.normal.z != 1.0 {
                return None;
            }

            Some(
                Circle {
                    center: point_from_dxf_point(&circle.center),
                    radius: circle.radius,
                }
                .to_path(DEFAULT_ACCURACY),
            )
        }
        EntityType::Ellipse(ref ellipse) => {
            // FIXME: currently only support viewing from +Z.
            if ellipse.normal.z != 1.0 {
                return None;
            }

            let center = point_from_dxf_point(&ellipse.center);
            let major_axis = Vec2 {
                x: ellipse.major_axis.x,
                y: -ellipse.major_axis.y,
            };
            let major_radius = major_axis.hypot();
            let minor_radius = major_radius * ellipse.minor_axis_ratio;
            Some(
                Arc {
                    center,
                    radii: Vec2 {
                        x: major_radius,
                        y: minor_radius,
                    },
                    start_angle: -ellipse.start_parameter,
                    sweep_angle: -(ellipse.end_parameter - ellipse.start_parameter)
                        .rem_euclid(2.0 * std::f64::consts::PI),
                    x_rotation: major_axis.angle(),
                }
                .to_path(DEFAULT_ACCURACY),
            )
        }
        EntityType::LwPolyline(ref lwp) => {
            // FIXME: currently only support viewing from +Z.
            if lwp.extrusion_direction.z != 1.0 {
                return None;
            }

            fn lwp_vertex_to_point(
                dxf::LwPolylineVertex { x, y, .. }: dxf::LwPolylineVertex,
            ) -> Point {
                Point { x, y: -y }
            }

            if lwp.vertices.len() < 2 {
                return None;
            }

            let mut bp = BezPath::new();
            bp.push(PathEl::MoveTo(lwp_vertex_to_point(lwp.vertices[0])));

            for w in lwp.vertices.windows(2) {
                let current = &w[0];
                let next = &w[1];
                let start = lwp_vertex_to_point(*current);
                let end = lwp_vertex_to_point(*next);

                // Bulge needs reversed because DXF is y-up
                let bulge = -current.bulge;
                add_poly_segment(&mut bp, start, end, bulge);
            }

            if lwp.is_closed() {
                bp.close_path();
            }

            Some(bp)
        }
        EntityType::Polyline(ref pl) => {
            // FIXME: currently only support viewing from +Z.
            if pl.normal.z != 1.0 {
                return None;
            }

            use dxf::entities::Vertex;
            // FIXME: Polyline variable width and arcs, and a variety of other things.
            //        In some cases vertices might actually be indices?
            if pl.is_polyface_mesh() || pl.is_3d_polygon_mesh() {
                return None;
            }

            let vertices: Vec<&Vertex> = pl.vertices().collect();
            if vertices.len() < 2 {
                return None;
            }

            let mut bp = BezPath::new();
            bp.push(PathEl::MoveTo(point_from_dxf_point(&vertices[0].location)));

            for w in vertices.windows(2) {
                let current = &w[0];
                let next = &w[1];
                let start = point_from_dxf_point(&current.location);
                let end = point_from_dxf_point(&next.location);

                // Bulge needs reversed because DXF is y-up
                let bulge = -current.bulge;
                add_poly_segment(&mut bp, start, end, bulge);
            }

            if pl.is_closed() {
                bp.close_path();
            }

            Some(bp)
        }
        EntityType::Spline(ref s) => {
            // FIXME: currently only support viewing from +Z.
            if s.normal.z != 1.0 {
                return None;
            }

            let degree = s.degree_of_curve as usize;
            if degree > 3 {
                // Splines of degree > 3 are not supported.
                return None;
            }

            let control_points: Vec<Point> =
                s.control_points.iter().map(point_from_dxf_point).collect();
            if control_points.len() < degree + 1 {
                return None;
            }

            let knots = &s.knot_values;
            if knots.len() < control_points.len() + degree + 1 {
                return None;
            }

            // Find unique knot spans within the valid range.
            let unique_knots: Vec<f64> = knots[degree..=(knots.len() - 1 - degree)]
                .iter()
                .copied()
                .map(OrdF64)
                .collect::<BTreeSet<_>>()
                .into_iter()
                .map(|OrdF64(k)| k)
                .collect();

            if unique_knots.is_empty() {
                return None;
            }

            let mut bp = BezPath::new();

            // Start at the first knot
            let first_point = eval_spline(degree, &control_points, knots, unique_knots[0]);
            bp.move_to(first_point);

            for w in unique_knots.windows(2) {
                let u0 = w[0];
                let u1 = w[1];
                match degree {
                    1 => {
                        let p1 = eval_spline(degree, &control_points, knots, u1);
                        bp.line_to(p1);
                    }
                    2 => {
                        let p0 = bp.elements().last().unwrap().end_point().unwrap();
                        let p2 = eval_spline(degree, &control_points, knots, u1);
                        let (dp, dcp, dk) =
                            derivative_control_points(degree, &control_points, knots);
                        let d0 = eval_spline(dp, &dcp, &dk, u0).to_vec2();
                        let d1 = eval_spline(dp, &dcp, &dk, u1).to_vec2();
                        if let Some(p1) = line_intersection(p0, d0, p2, d1) {
                            bp.quad_to(p1, p2);
                        } else {
                            // Parallel tangents.
                            bp.line_to(p2);
                        }
                    }
                    3 => {
                        let p0 = bp.elements().last().unwrap().end_point().unwrap();
                        let p3 = eval_spline(degree, &control_points, knots, u1);
                        let (dp, dcp, dk) =
                            derivative_control_points(degree, &control_points, knots);
                        let d0 = eval_spline(dp, &dcp, &dk, u0);
                        let d1 = eval_spline(dp, &dcp, &dk, u1);
                        let delta_u = u1 - u0;
                        let p1 = Point {
                            x: p0.x + (delta_u / 3.0) * d0.x,
                            y: p0.y + (delta_u / 3.0) * d0.y,
                        };
                        let p2 = Point {
                            x: p3.x - (delta_u / 3.0) * d1.x,
                            y: p3.y - (delta_u / 3.0) * d1.y,
                        };
                        bp.curve_to(p1, p2, p3);
                    }
                    _ => unreachable!(), // Degrees > 3 filtered earlier.
                }
            }

            if s.is_closed() {
                bp.close_path();
            }

            Some(bp)
        }
        EntityType::Solid(ref s) => {
            // FIXME: currently only support viewing from +Z.
            if s.extrusion_direction.z != 1.0 {
                return None;
            }

            let mut bp = BezPath::new();
            bp.move_to(point_from_dxf_point(&s.first_corner));
            bp.line_to(point_from_dxf_point(&s.third_corner));
            if s.third_corner != s.fourth_corner {
                bp.line_to(point_from_dxf_point(&s.fourth_corner));
            }
            bp.line_to(point_from_dxf_point(&s.second_corner));
            bp.close_path();
            Some(bp)
        }
        _ => {
            let specific = dxf_entity_type_name(&e.specific);
            tracing::trace!(entity=e.common.handle.0, layer=e.common.layer, type=specific, "unhandled");
            None
        }
    }
}

// `f64` doesn't implement `Ord`, this is less ugly than other solutions.
#[derive(PartialEq)]
struct OrdF64(f64);
impl Eq for OrdF64 {}
impl PartialOrd for OrdF64 {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for OrdF64 {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.total_cmp(&other.0)
    }
}

/// Evaluate a B-spline at `u`.
fn eval_spline(degree: usize, control_points: &[Point], knots: &[f64], u: f64) -> Point {
    let n = control_points.len() - 1;
    let k = knots
        .iter()
        .position(|&knot| knot > u)
        .unwrap_or(knots.len() - 1)
        .saturating_sub(1);
    if k < degree || k > n {
        return if u < knots[degree] {
            control_points[0]
        } else {
            control_points[n]
        };
    }
    let mut d = control_points[k - degree..=k].to_vec();
    for r in 1..=degree {
        for i in (r..=degree).rev() {
            let alpha = (u - knots[k - degree + i])
                / (knots[k - degree + i + degree - r + 1] - knots[k - degree + i]);
            d[i] = Point {
                x: (1.0 - alpha) * d[i - 1].x + alpha * d[i].x,
                y: (1.0 - alpha) * d[i - 1].y + alpha * d[i].y,
            };
        }
    }
    d[degree]
}

/// Compute derivative control points and knots.
fn derivative_control_points(
    degree: usize,
    control_points: &[Point],
    knots: &[f64],
) -> (usize, Vec<Point>, Vec<f64>) {
    let n = control_points.len() - 1;
    if degree == 0 || n < 1 {
        return (0, vec![], knots.to_vec());
    }
    let new_degree = degree - 1;
    let new_control_points: Vec<Point> = (0..n)
        .map(|i| {
            let factor = degree as f64 / (knots[i + degree + 1] - knots[i + 1]);
            let diff = control_points[i + 1] - control_points[i];
            Point {
                x: factor * diff.x,
                y: factor * diff.y,
            }
        })
        .collect();
    let new_knots = knots[1..knots.len() - 1].to_vec();
    (new_degree, new_control_points, new_knots)
}

/// Find the intersection of infinite lines p0 + t × d0 and p1 + t × d1.
fn line_intersection(p0: Point, d0: Vec2, p1: Point, d1: Vec2) -> Option<Point> {
    let determinant = d0.x * -d1.y - -d1.x * d0.y;
    if determinant.abs() < 1e-10 {
        // Effectively parallel.
        None
    } else {
        let t = ((p1.x - p0.x) * -d1.y - (p1.y - p0.y) * -d1.x) / determinant;
        Some(Point {
            x: p0.x + t * d0.x,
            y: p0.y + t * d0.y,
        })
    }
}

/// Add a polyline segment to a `BezPath`, taking bulge into account.
fn add_poly_segment(bp: &mut BezPath, start: Point, end: Point, bulge: f64) {
    if bulge == 0.0 {
        bp.push(PathEl::LineTo(end));
        return;
    }

    let theta = 4.0 * bulge.atan();
    if theta.abs() < 1e-6 {
        bp.push(PathEl::LineTo(end));
        return;
    }

    let v = end - start;
    let d = v.hypot();
    if d < 1e-10 {
        // Points are too dang close.
        bp.push(PathEl::LineTo(end));
        return;
    }

    let r = d / (2.0 * (theta / 2.0).sin().abs());

    let center = {
        let s = bulge.signum();
        let perp = Vec2 {
            x: -s * v.y,
            y: s * v.x,
        };
        let h = r * (theta / 2.0).cos();
        let midpoint = (start.to_vec2() + end.to_vec2()) / 2.0;
        (midpoint + (h / d) * perp).to_point()
    };

    let start_angle = (start - center.to_vec2()).to_vec2().atan2();

    let arc = Arc {
        center,
        radii: Vec2 { x: r, y: r },
        start_angle,
        sweep_angle: theta,
        x_rotation: 0.0,
    };

    arc.to_cubic_beziers(DEFAULT_ACCURACY, |p1, p2, p3| {
        bp.curve_to(p1, p2, p3);
    });
}

/// Make a [`Point`] from the x and y of a [`dxf::Point`].
pub fn point_from_dxf_point(p: &dxf::Point) -> Point {
    let dxf::Point { x, y, .. } = *p;
    Point { x, y: -y }
}

/// Provide information about a drawing after loading it.
#[allow(
    missing_debug_implementations,
    reason = "Not particularly useful, and members don't implement Debug."
)]
pub struct DrawingInfo {
    drawing: Drawing,
}

impl DrawingInfo {
    pub(crate) fn new(drawing: Drawing) -> Self {
        Self { drawing }
    }

    /// Get an entity in the drawing.
    pub fn get_entity(&self, eh: EntityHandle) -> &dxf::entities::Entity {
        let dxf::DrawingItem::Entity(e) = self
            .drawing
            .item_by_handle(dxf::Handle(eh.0.get()))
            .unwrap()
        else {
            unreachable!();
        };
        e
    }
}

/// Adapt line weights to [`FatPaint`] strokes for rendering.
#[derive(Debug, Clone, Copy)]
pub struct RestrokePaint {
    /// Physical line weight expressed in [iota][`joto_constants::u64::IOTA`].
    pub weight: u64,
    /// The target [`PaintHandle`].
    pub handle: PaintHandle,
}

impl RestrokePaint {
    /// Adapt line weight to a device.
    ///
    /// For legacy reasons many lines in drawings are 0 weight.
    /// The expectation of interactive applications is that lines with 0 weight are
    /// displayed as one display pixel wide, and although ambiguous, it seems that
    /// all lines are expected to be displayed at least one display pixel wide.
    /// Therefore, `min_stroke` should be the width of a 1 device pixel stroke at
    /// default scale.
    ///
    /// For modern printing, you will need to decide on a `min_stroke` that makes
    /// sense for your printer, assumptions in drawings come from robotic plotters.
    ///
    /// For reference, see the [AutoCAD documentation for line weights][0].
    ///
    /// * `graphics` — the [`GraphicsBag`] that contains the paints to be updated.
    /// * `pitch` — Physical pitch of a 1.0 stroke, generally 1 display pixel, in [iota][`joto_constants::u64::IOTA`].
    /// * `view_scale` — uniform scale of the drawing view transform.
    /// * `min_stroke` — minimum stroke width, typically 1 device pixel.
    /// * `max_stroke` — maximum stroke width, useful for plotters.
    ///
    /// [0]: https://help.autodesk.com/view/ACD/2025/ENU/?guid=GUID-4B33ACD3-F6DD-4CB5-8C55-D6D0D7130905
    pub fn adapt(
        &self,
        graphics: &mut GraphicsBag,
        pitch: u64,
        view_scale: f64,
        min_stroke: f64,
        max_stroke: f64,
    ) {
        let pxw = (self.weight as f64 / pitch as f64).clamp(min_stroke, max_stroke);
        let p = graphics.get_paint_mut(self.handle);
        p.stroke = Stroke::new(pxw / view_scale);
    }
}

impl From<(u64, PaintHandle)> for RestrokePaint {
    fn from((weight, handle): (u64, PaintHandle)) -> Self {
        Self { weight, handle }
    }
}

/// Tabulon data for the drawing.
#[allow(
    missing_debug_implementations,
    reason = "Not particularly useful, and members don't implement Debug."
)]
pub struct TDDrawing {
    /// `GraphicsBag` containing drawn items.
    pub graphics: GraphicsBag,
    /// Mapping from graphics items to entity handles.
    pub item_entity_map: BTreeMap<ItemHandle, EntityHandle>,
    /// Entities for layers.
    pub entity_layer_map: BTreeMap<EntityHandle, LayerHandle>,
    /// Render layer in drawing order.
    pub render_layer: RenderLayer,
    /// Enabled layers.
    pub enabled_layers: BTreeSet<LayerHandle>,
    /// Layer names.
    pub layer_names: BTreeMap<LayerHandle, sync::Arc<str>>,
    /// Drawing information object.
    pub info: DrawingInfo,
    /// Paints that need stroke widths computed relative to view.
    ///
    /// See [`RestrokePaint`].
    pub restroke_paints: sync::Arc<[RestrokePaint]>,
}

use parley::{FontStyle, FontWeight, FontWidth, GenericFamily, StyleProperty};

/// Check if the font size of a [`StyleSet`] is zero.
fn style_size_is_zero(s: &StyleSet<Option<Color>>) -> bool {
    s.inner()
        .get(&core::mem::discriminant(&StyleProperty::FontSize(0_f32)))
        .is_none_or(|x| matches!(x, StyleProperty::FontSize(0_f32)))
}

/// Recover color enum value from [`dxf::Color`] as it is currently not in the API.
fn recover_color_enum(c: &dxf::Color) -> i16 {
    if c.is_by_layer() {
        256
    } else if c.is_by_entity() {
        257
    } else if c.is_by_block() {
        0
    } else if let Some(index) = c.index() {
        index as i16
    } else {
        -1
    }
}

/// Load a DXF from a path into a [`TDDrawing`].
#[cfg(feature = "std")]
#[tracing::instrument(skip_all)]
pub fn load_file_default_layers(path: impl AsRef<Path>) -> DxfResult<TDDrawing> {
    let mut gb = GraphicsBag::default();
    let mut rl = RenderLayer::default();
    let mut item_entity_map = BTreeMap::new();
    let mut entity_layer_map = BTreeMap::new();

    // FIXME: use real colors and line widths, and expose information for line scaling.
    //        This currently sets the paint at position 0/default in the palette.
    let _paint = gb.register_paint(FatPaint {
        stroke: Default::default(),
        stroke_paint: Some(Color::BLACK.into()),
        fill_paint: None,
    });

    let drawing = Drawing::load_file(path)?;

    let visible_layers: BTreeSet<&str> = drawing
        .layers()
        .filter_map(|l| l.is_layer_on.then_some(l.name.as_str()))
        .collect();

    let enabled_layers = drawing
        .layers()
        .filter_map(|l| {
            l.is_layer_on
                .then_some(LayerHandle(NonZeroU64::new(l.handle.0).unwrap()))
        })
        .collect();

    let layer_names = drawing
        .layers()
        .map(|l| {
            (
                LayerHandle(NonZeroU64::new(l.handle.0).unwrap()),
                l.name.as_str().into(),
            )
        })
        .collect();

    let handle_for_layer_name: BTreeMap<&str, LayerHandle> = drawing
        .layers()
        .map(|l| {
            (
                l.name.as_str(),
                LayerHandle(NonZeroU64::new(l.handle.0).unwrap()),
            )
        })
        .collect();

    let layers: BTreeMap<LayerHandle, &dxf::tables::Layer> = drawing
        .layers()
        .map(|l| (LayerHandle(NonZeroU64::new(l.handle.0).unwrap()), l))
        .collect();

    let styles: BTreeMap<&str, StyleSet<Option<Color>>> = drawing
        .styles()
        .map(
            #[allow(clippy::cast_possible_truncation, reason = "It doesn't matter")]
            |s| {
                // FIXME: I'm told this is actually the cap height and not the em size,
                //        at least for shx line fonts.
                // When this is zero, the height from the TEXT/MTEXT entity is used;
                // when this is nonzero, the height from the TXT/MTEXT is ignored.
                let size = s.text_height;
                let mut pstyle: StyleSet<Option<Color>> = StyleSet::new(size as f32);
                pstyle.insert(StyleProperty::LineHeight(LineHeight::FontSizeRelative(1.0)));
                pstyle.insert(StyleProperty::FontWidth(FontWidth::from_ratio(
                    s.width_factor as f32,
                )));
                if s.oblique_angle != 0.0 {
                    pstyle.insert(StyleProperty::FontStyle(FontStyle::Oblique(Some(
                        s.oblique_angle as f32,
                    ))));
                }

                // TODO: Handle text_generation_flags somehow; My understanding is:
                //        - The second bit means the text is mirrored lengthwise
                //        - The third bit means the text is mirrored vertically

                // This is a selection of shx file names I've seen in the wild.
                //
                // TODO: We should probably eventually map to more correct fonts, or
                //       somehow match the outer metrics of these fonts more closely.
                //
                //       Sometimes the file names have the .shx, sometimes they do not,
                //       there appears to be neither rhyme nor reason to it.
                match s.primary_font_file_name.as_str() {
                    // Monospace version of txt.shx
                    "monotxt" | "monotxt.shx" => pstyle.insert(GenericFamily::Monospace.into()),
                    // Italic roman type lined once.
                    "italic" | "italic.shx" => {
                        pstyle.insert(GenericFamily::Serif.into());
                        pstyle.insert(StyleProperty::FontStyle(FontStyle::Italic))
                    }
                    // Roman (serif) type lined once.
                    "romans" | "romans.shx" => pstyle.insert(GenericFamily::Serif.into()),
                    // Condensed Roman type lined once.
                    "romanc" | "romanc.shx" => {
                        pstyle.insert(GenericFamily::Serif.into());
                        pstyle.insert(StyleProperty::FontWidth(FontWidth::CONDENSED))
                    }
                    // Roman type lined twice, seems like bold.
                    "romand" | "romand.shx" => {
                        pstyle.insert(GenericFamily::Serif.into());
                        pstyle.insert(StyleProperty::FontWeight(FontWeight::BOLD))
                    }
                    // Roman type lined thrice, seems like bolder.
                    "romant" | "romant.shx" => {
                        pstyle.insert(GenericFamily::Serif.into());
                        pstyle.insert(StyleProperty::FontWeight(FontWeight::EXTRA_BOLD))
                    }
                    "script" | "script.shx" => pstyle.insert(GenericFamily::Cursive.into()),
                    // Covers common "txt" | "txt.shx" | "simplex.shx" | "isocp.shx" | "gothic.shx"
                    _ => pstyle.insert(GenericFamily::SansSerif.into()),
                };

                (s.name.as_str(), pstyle)
            },
        )
        .collect();

    let mut blocks: BTreeMap<&str, Vec<BlockChunk>> = BTreeMap::new();
    {
        // Blocks that depend on another block which is not realized.
        let mut unresolved_blocks: Vec<&dxf::Block> = drawing.blocks().collect();
        let mut there_is_absolutely_no_hope = false;
        while !unresolved_blocks.is_empty() && !there_is_absolutely_no_hope {
            // I acknowledge that this is technically not very efficient in some cases
            // but I am too lazy to build a DAG here, and rarely will it matter.
            there_is_absolutely_no_hope = true;
            'block: for b in unresolved_blocks.iter() {
                // Form up shapes with contiguous line weight and color.
                let mut lines = BezPath::new();
                // Chunk blocks by the combination of line weight and color.
                // To retain drawing order, multiple chunks may be emitted for a single block.
                let mut chunks: Vec<BlockChunk> = vec![];
                if b.entities.is_empty() {
                    blocks.insert(b.name.as_str(), chunks);
                    continue;
                }

                let resolve_style = |lh: LayerHandle, lw: i16, ce: i16| {
                    let layer = layers[&lh];
                    let line_weight = if lw == -2 {
                        if layer.line_weight.raw_value() < 0 {
                            25_i16
                        } else {
                            layer.line_weight.raw_value()
                        }
                    } else {
                        lw
                    };
                    let color = if ce == 256 {
                        // BYLAYER: resolve to a palette value during block resolution.
                        if let Some(i) = layer.color.index() {
                            i as i16
                        } else {
                            // white if layer doesn't have a resolvable color.
                            7_i16
                        }
                    } else {
                        ce
                    };

                    (line_weight, color)
                };

                let mut cur_style = resolve_style(
                    handle_for_layer_name[b.entities[0].common.layer.as_str()],
                    b.entities[0].common.lineweight_enum_value,
                    recover_color_enum(&b.entities[0].common.color),
                );

                for e in b.entities.iter() {
                    let lh = handle_for_layer_name[e.common.layer.as_str()];
                    let style = resolve_style(
                        lh,
                        if matches!(e.specific, EntityType::Solid(..)) {
                            // Use `i16::MIN` for solid fills.
                            i16::MIN
                        } else {
                            e.common.lineweight_enum_value
                        },
                        recover_color_enum(&e.common.color),
                    );
                    if style != cur_style {
                        chunks.push(BlockChunk::Path(cur_style.0, cur_style.1, lines));
                        lines = BezPath::new();
                        cur_style = style;
                    }

                    match e.specific {
                        // Try the next block if this one depends on an unresolved block.
                        EntityType::Insert(dxf::entities::Insert { ref name, .. })
                            if !blocks.contains_key(name.as_str()) =>
                        {
                            continue 'block;
                        }
                        EntityType::Insert(ref ins) => {
                            // FIXME: currently only support viewing from +Z.
                            if ins.extrusion_direction.z != 1.0 {
                                continue;
                            }
                            if let Some(b) = blocks.get(ins.name.as_str()) {
                                let base_transform = Affine::scale_non_uniform(
                                    ins.x_scale_factor,
                                    ins.y_scale_factor,
                                );
                                let location = point_from_dxf_point(&ins.location);

                                if !lines.is_empty() {
                                    // Always push a chunk before an insert if not empty.
                                    chunks.push(BlockChunk::Path(cur_style.0, cur_style.1, lines));
                                }

                                for chunk in b {
                                    match chunk {
                                        BlockChunk::Path(lw, ce, clines) => {
                                            let local_linewidth = if *lw == -1 {
                                                // BYBLOCK: inherit from this insert.
                                                cur_style.0
                                            } else {
                                                // Other values are already realized in the chunk as
                                                // either absolute widths, or the default width `-3`.
                                                *lw
                                            };
                                            let local_color = if *ce == 0 {
                                                // BYBLOCK: inherit from this insert.
                                                cur_style.1
                                            } else {
                                                // Other values are already realized in the chunk.
                                                *ce
                                            };
                                            lines = BezPath::new();
                                            for i in 0..ins.row_count {
                                                for j in 0..ins.column_count {
                                                    let transform = base_transform
                                                        .then_translate(Vec2::new(
                                                            j as f64 * ins.column_spacing,
                                                            i as f64 * ins.row_spacing,
                                                        ))
                                                        .then_rotate(-ins.rotation.to_radians())
                                                        .then_translate(location.to_vec2());
                                                    // Add the transformed instance to the new path.
                                                    lines.extend(transform * clines);
                                                }
                                            }
                                            chunks.push(BlockChunk::Path(
                                                local_linewidth,
                                                local_color,
                                                lines,
                                            ));
                                        }
                                        #[allow(
                                            clippy::cast_possible_truncation,
                                            reason = "It doesn't matter"
                                        )]
                                        BlockChunk::Text(ce, ti) => {
                                            let TextItem {
                                                text,
                                                style,
                                                alignment,
                                                insertion,
                                                max_inline_size,
                                                attachment_point,
                                            } = ti.clone();

                                            let local_color = if *ce == 0 {
                                                // BYBLOCK: inherit from this insert.
                                                cur_style.1
                                            } else {
                                                // Other values are already realized in the chunk.
                                                *ce
                                            };

                                            let fs = style
                                                .inner()
                                                .get(&discriminant(&StyleProperty::FontSize(0.0)))
                                                .map(|p| {
                                                    if let StyleProperty::FontSize(fs) = p {
                                                        *fs
                                                    } else {
                                                        0.0
                                                    }
                                                });

                                            let displacement_point =
                                                insertion.displacement.to_point();
                                            let baseline_vector = Vec2::from_angle(insertion.angle);
                                            let height_vector = Vec2 {
                                                x: -baseline_vector.y,
                                                y: baseline_vector.x,
                                            };
                                            let baseline = Line {
                                                p0: displacement_point,
                                                p1: displacement_point
                                                    + (baseline_vector
                                                        * max_inline_size.unwrap_or(1.0) as f64),
                                            };
                                            let hline = Line {
                                                p0: displacement_point,
                                                p1: displacement_point
                                                    + (height_vector * fs.unwrap_or(1.0) as f64),
                                            };

                                            for i in 0..ins.row_count {
                                                for j in 0..ins.column_count {
                                                    let transform = base_transform
                                                        .then_translate(Vec2::new(
                                                            j as f64 * ins.column_spacing,
                                                            i as f64 * ins.row_spacing,
                                                        ))
                                                        .then_rotate(-ins.rotation.to_radians())
                                                        .then_translate(location.to_vec2());

                                                    let tbline = transform * baseline;
                                                    let thline = transform * hline;

                                                    let angle = (tbline.p1 - tbline.p0).angle();
                                                    let displacement = tbline.p0.to_vec2();

                                                    let mut style = style.clone();
                                                    let nfs = thline.length();
                                                    if fs.is_some() {
                                                        style.insert(StyleProperty::FontSize(
                                                            nfs as f32,
                                                        ));
                                                    }

                                                    chunks.push(BlockChunk::Text(
                                                        local_color,
                                                        TextItem {
                                                            text: text.clone(),
                                                            style,
                                                            alignment,
                                                            insertion: DirectIsometry {
                                                                angle,
                                                                displacement,
                                                            },
                                                            max_inline_size: max_inline_size
                                                                .map(|_| tbline.length() as f32),
                                                            attachment_point,
                                                        },
                                                    ));
                                                }
                                            }
                                        }
                                        _ => {}
                                    }
                                }
                                lines = BezPath::new();
                            }
                        }
                        _ => match chunk_from_entity(e, &styles) {
                            BlockChunk::Path(_, _, s) => {
                                lines.extend(s);
                            }
                            BlockChunk::Text(_, ti) => {
                                // eprintln!("ENCOUNTERED TEXT IN BLOCK DEFINITION");
                                // eprintln!("{e:#?}");
                                // eprintln!("{ti:#?}");
                                if !lines.is_empty() {
                                    chunks.push(BlockChunk::Path(cur_style.0, cur_style.1, lines));
                                    lines = BezPath::new();
                                }
                                chunks.push(BlockChunk::Text(cur_style.1, ti));
                            }
                            BlockChunk::Unsupported => {
                                if !lines.is_empty() {
                                    chunks.push(BlockChunk::Path(cur_style.0, cur_style.1, lines));
                                    lines = BezPath::new();
                                }
                                chunks.push(BlockChunk::Unsupported);
                            }
                        },
                    }
                }
                if !lines.is_empty() {
                    chunks.push(BlockChunk::Path(cur_style.0, cur_style.1, lines));
                }
                there_is_absolutely_no_hope = false;
                blocks.insert(b.name.as_str(), chunks);
            }
            unresolved_blocks.retain(|b| !blocks.contains_key(b.name.as_str()));
        }
    }

    // Paints keyed on concrete rgba color, and concrete line width (in iotas).
    let mut paints: BTreeMap<(u32, u64), PaintHandle> = BTreeMap::new();
    let mut fills: BTreeMap<u32, PaintHandle> = BTreeMap::new();

    for e in drawing.entities() {
        if !e.common.is_visible
            || !(e.common.layer.is_empty() || visible_layers.contains(e.common.layer.as_str()))
        {
            continue;
        }

        let eh = EntityHandle(NonZeroU64::new(e.common.handle.0).unwrap());
        let lh = handle_for_layer_name[e.common.layer.as_str()];

        let layer = layers[&lh];

        let mut resolve_paint = |gb: &mut GraphicsBag, lw: i16, c: i16| {
            // Resolve color.
            let opaque_color = match c {
                // BYENTITY
                257 => e.common.color_24_bit as u32,
                // BYLAYER
                256 => {
                    if let Some(i) = layer.color.index() {
                        ACI[i as usize]
                    } else {
                        u32::MAX
                    }
                }
                // Indexed colors.
                1..=255 => ACI[c as usize],
                // Other values generally not valid in this context.
                _ => u32::MAX,
            };
            let combined_color =
                (opaque_color << 8) | (0xFF - (e.common.transparency as u32 & 0xFF));

            /// Default line weight.
            const LWDEFAULT: u64 = 250 * MICROMETER;

            // Resolve line width.
            let lwconcrete = match lw {
                -3 => LWDEFAULT,
                // BYLAYER.
                -2 => {
                    if layer.line_weight.raw_value() <= 0 {
                        // BYLAYER and BYBLOCK are both meaningless in a layer,
                        // therefore, use the default for all enumerations.
                        LWDEFAULT
                    } else {
                        layer.line_weight.raw_value() as u64 * 10 * MICROMETER
                    }
                }
                // BYBLOCK (-1) Should not occur at the entity level, use default.
                //
                // Other negative values occur in the wild but have no standard
                // meaning, as such all negative values not specifically handled
                // above should have the default line width.
                i if i < 0 => LWDEFAULT,
                i => i as u64 * 10 * MICROMETER,
            };

            let r = ((combined_color >> 24) & 0xFF) as u8;
            let g = ((combined_color >> 16) & 0xFF) as u8;
            let b = ((combined_color >> 8) & 0xFF) as u8;
            let a = (combined_color & 0xFF) as u8;

            if lw == i16::MIN {
                // `i16::MIN` reserved for solid fills
                *fills.entry(combined_color).or_insert_with(|| {
                    gb.register_paint(FatPaint {
                        fill_paint: Some(Color::from_rgba8(r, g, b, a).into()),
                        ..Default::default()
                    })
                })
            } else {
                *paints
                    .entry((combined_color, lwconcrete))
                    .or_insert_with(|| {
                        // At first these do not have stroke width, this needs to be set afterward.
                        gb.register_paint(FatPaint {
                            stroke_paint: Some(Color::from_rgba8(r, g, b, a).into()),
                            ..Default::default()
                        })
                    })
            }
        };

        // Get or create the appropriate PaintHandle for this entity.
        let entity_paint = resolve_paint(
            &mut gb,
            if matches!(
                e.specific,
                EntityType::Solid(..) | EntityType::Text(..) | EntityType::MText(..)
            ) {
                // Use `i16::MIN` for solid fills.
                i16::MIN
            } else {
                e.common.lineweight_enum_value
            },
            recover_color_enum(&e.common.color),
        );

        let mut push_item = |gb: &mut GraphicsBag, item: GraphicsItem| {
            let ih = rl.push_with_bag(gb, item);
            item_entity_map.insert(ih, eh);
            entity_layer_map.insert(eh, lh);
        };

        match e.specific {
            EntityType::Insert(ref ins) => {
                // FIXME: currently only support viewing from +Z.
                if ins.extrusion_direction.z != 1.0 {
                    continue;
                }

                if let Some(b) = blocks.get(ins.name.as_str()) {
                    let base_transform =
                        Affine::scale_non_uniform(ins.x_scale_factor, ins.y_scale_factor);
                    let location = point_from_dxf_point(&ins.location);

                    for chunk in b {
                        match chunk {
                            BlockChunk::Path(lw, ce, clines) => {
                                let chunk_paint = resolve_paint(
                                    &mut gb,
                                    if *lw == -1 {
                                        // BYBLOCK: inherit from this insert.
                                        e.common.lineweight_enum_value
                                    } else {
                                        *lw
                                    },
                                    if *ce == 0 {
                                        // BYBLOCK: inherit from this insert.
                                        recover_color_enum(&e.common.color)
                                    } else {
                                        *ce
                                    },
                                );
                                let mut path = BezPath::new();
                                for i in 0..ins.row_count {
                                    for j in 0..ins.column_count {
                                        let transform = base_transform
                                            .then_translate(Vec2::new(
                                                j as f64 * ins.column_spacing,
                                                i as f64 * ins.row_spacing,
                                            ))
                                            .then_rotate(-ins.rotation.to_radians())
                                            .then_translate(location.to_vec2());

                                        path.extend(transform * clines);
                                    }
                                }
                                push_item(
                                    &mut gb,
                                    FatShape {
                                        path: sync::Arc::from(path),
                                        paint: chunk_paint,
                                        ..Default::default()
                                    }
                                    .into(),
                                );
                            }
                            #[allow(clippy::cast_possible_truncation, reason = "It doesn't matter")]
                            BlockChunk::Text(ce, ti) => {
                                let TextItem {
                                    text,
                                    style,
                                    alignment,
                                    insertion,
                                    max_inline_size,
                                    attachment_point,
                                } = ti.clone();

                                let fs = style
                                    .inner()
                                    .get(&discriminant(&StyleProperty::FontSize(0.0)))
                                    .map(|p| {
                                        if let StyleProperty::FontSize(fs) = p {
                                            *fs
                                        } else {
                                            0.0
                                        }
                                    });

                                let displacement_point = insertion.displacement.to_point();
                                let baseline_vector = Vec2::from_angle(insertion.angle);
                                let height_vector = Vec2 {
                                    x: -baseline_vector.y,
                                    y: baseline_vector.x,
                                };
                                let baseline = Line {
                                    p0: displacement_point,
                                    p1: displacement_point
                                        + (baseline_vector * max_inline_size.unwrap_or(1.0) as f64),
                                };
                                let hline = Line {
                                    p0: displacement_point,
                                    p1: displacement_point
                                        + (height_vector * fs.unwrap_or(1.0) as f64),
                                };

                                let paint = resolve_paint(
                                    &mut gb,
                                    i16::MIN,
                                    if *ce == 0 {
                                        recover_color_enum(&e.common.color)
                                    } else {
                                        *ce
                                    },
                                );

                                for i in 0..ins.row_count {
                                    for j in 0..ins.column_count {
                                        let transform = base_transform
                                            .then_translate(Vec2::new(
                                                j as f64 * ins.column_spacing,
                                                i as f64 * ins.row_spacing,
                                            ))
                                            .then_rotate(-ins.rotation.to_radians())
                                            .then_translate(location.to_vec2());

                                        let tbline = transform * baseline;
                                        let thline = transform * hline;

                                        let angle = (tbline.p1 - tbline.p0).angle();
                                        let displacement = tbline.p0.to_vec2();

                                        let mut style = style.clone();
                                        let nfs = thline.length();
                                        if fs.is_some() {
                                            style.insert(StyleProperty::FontSize(nfs as f32));
                                        }

                                        push_item(
                                            &mut gb,
                                            FatText {
                                                transform: Default::default(),
                                                paint,
                                                text: text.clone(),
                                                style: style.clone(),
                                                alignment,
                                                insertion: DirectIsometry {
                                                    angle,
                                                    displacement,
                                                },
                                                max_inline_size: max_inline_size
                                                    .map(|_| tbline.length() as f32),
                                                attachment_point,
                                            }
                                            .into(),
                                        );
                                    }
                                }
                            }
                            BlockChunk::Unsupported => {
                                // Maybe do something idk.
                            }
                        }
                    }
                }
            }
            _ => match chunk_from_entity(e, &styles) {
                BlockChunk::Path(_, _, s) => {
                    push_item(
                        &mut gb,
                        FatShape {
                            path: sync::Arc::from(s),
                            paint: entity_paint,
                            ..Default::default()
                        }
                        .into(),
                    );
                }
                BlockChunk::Text(_, ti) => {
                    let TextItem {
                        text,
                        style,
                        alignment,
                        insertion,
                        max_inline_size,
                        attachment_point,
                    } = ti.clone();

                    push_item(
                        &mut gb,
                        FatText {
                            transform: Default::default(),
                            paint: entity_paint,
                            text,
                            style,
                            alignment,
                            insertion,
                            max_inline_size,
                            attachment_point,
                        }
                        .into(),
                    );
                }
                _ => {}
            },
        }
    }

    let restroke_paints: Vec<RestrokePaint> =
        paints.iter().map(|((_, w), h)| (*w, *h).into()).collect();

    Ok(TDDrawing {
        graphics: gb,
        render_layer: rl,
        item_entity_map,
        entity_layer_map,
        enabled_layers,
        layer_names,
        info: DrawingInfo::new(drawing),
        restroke_paints: sync::Arc::from(restroke_paints.as_slice()),
    })
}

/// Convert a [`dxf::enums::AttachmentPoint`] to a [`tabulon::text::AttachmentPoint`].
fn dxf_attachment_point_to_tabulon(
    attachment_point: dxf::enums::AttachmentPoint,
) -> AttachmentPoint {
    use AttachmentPoint::*;
    use dxf::enums::AttachmentPoint as d;
    match attachment_point {
        d::TopLeft => TopLeft,
        d::TopCenter => TopCenter,
        d::TopRight => TopRight,
        d::MiddleLeft => MiddleLeft,
        d::MiddleCenter => MiddleCenter,
        d::MiddleRight => MiddleRight,
        d::BottomLeft => BottomLeft,
        d::BottomCenter => BottomCenter,
        d::BottomRight => BottomRight,
    }
}

/// Get the type name of a DXF `EntityType`
fn dxf_entity_type_name(entity_type: &EntityType) -> &str {
    match entity_type {
        EntityType::Face3D(_) => "Face3D",
        EntityType::Solid3D(_) => "Solid3D",
        EntityType::ProxyEntity(_) => "ProxyEntity",
        EntityType::Arc(_) => "Arc",
        EntityType::ArcAlignedText(_) => "ArcAlignedText",
        EntityType::AttributeDefinition(_) => "AttributeDefinition",
        EntityType::Attribute(_) => "Attribute",
        EntityType::Body(_) => "Body",
        EntityType::Circle(_) => "Circle",
        EntityType::RotatedDimension(_) => "RotatedDimension",
        EntityType::RadialDimension(_) => "RadialDimension",
        EntityType::DiameterDimension(_) => "DiameterDimension",
        EntityType::AngularThreePointDimension(_) => "AngularThreePointDimension",
        EntityType::OrdinateDimension(_) => "OrdinateDimension",
        EntityType::Ellipse(_) => "Ellipse",
        EntityType::Helix(_) => "Helix",
        EntityType::Image(_) => "Image",
        EntityType::Insert(_) => "Insert",
        EntityType::Leader(_) => "Leader",
        EntityType::Light(_) => "Light",
        EntityType::Line(_) => "Line",
        EntityType::LwPolyline(_) => "LwPolyline",
        EntityType::MLine(_) => "MLine",
        EntityType::MText(_) => "MText",
        EntityType::OleFrame(_) => "OleFrame",
        EntityType::Ole2Frame(_) => "Ole2Frame",
        EntityType::ModelPoint(_) => "ModelPoint",
        EntityType::Polyline(_) => "Polyline",
        EntityType::Ray(_) => "Ray",
        EntityType::Region(_) => "Region",
        EntityType::RText(_) => "RText",
        EntityType::Section(_) => "Section",
        EntityType::Seqend(_) => "Seqend",
        EntityType::Shape(_) => "Shape",
        EntityType::Solid(_) => "Solid",
        EntityType::Spline(_) => "Spline",
        EntityType::Text(_) => "Text",
        EntityType::Tolerance(_) => "Tolerance",
        EntityType::Trace(_) => "Trace",
        EntityType::DgnUnderlay(_) => "DgnUnderlay",
        EntityType::DwfUnderlay(_) => "DwfUnderlay",
        EntityType::PdfUnderlay(_) => "PdfUnderlay",
        EntityType::Vertex(_) => "Vertex",
        EntityType::Wipeout(_) => "Wipeout",
        EntityType::XLine(_) => "XLine",
    }
}

#[cfg(test)]
mod tests {}
