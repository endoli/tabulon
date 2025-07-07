// Copyright 2025 the Tabulon Authors
// SPDX-License-Identifier: Apache-2.0 OR MIT

extern crate alloc;
use alloc::{vec, vec::Vec};

use core::num::NonZeroU32;

use crate::{
    shape::{FatPaint, FatShape},
    text::FatText,
};

use peniko::kurbo::Affine;

/// A handle for a transform.
#[derive(Debug, Default, Clone, Copy, PartialOrd, Ord, PartialEq, Eq)]
pub struct TransformHandle(Option<NonZeroU32>);

/// A handle for a `GraphicsItem` in a `GraphicsBag`.
#[derive(Debug, Default, Clone, Copy, PartialOrd, Ord, PartialEq, Eq)]
pub struct ItemHandle(u32);

/// A handle for a `FatPaint` in a `GraphicsBag`.
#[derive(Debug, Default, Clone, Copy, PartialOrd, Ord, PartialEq, Eq)]
pub struct PaintHandle(u32);

impl From<PaintHandle> for usize {
    fn from(h: PaintHandle) -> Self {
        h.0 as Self
    }
}

impl From<TransformHandle> for usize {
    fn from(h: TransformHandle) -> Self {
        h.0.map_or(0, |x| x.get() as Self)
    }
}

/// Transform record for deriving final transforms.
#[derive(Debug, Clone, Copy, Default)]
struct ManagedTransform {
    /// `TransformHandle` for the parent transform.
    pub(crate) parent: TransformHandle,
    pub(crate) local: Affine,
}

/// Items for [`GraphicsBag`].
#[derive(Debug)]
#[allow(
    clippy::large_enum_variant,
    reason = "Making FatShape more indirect doesn't help, and there is no other elegant way to handle this."
)]
pub enum GraphicsItem {
    /// See [`FatShape`].
    FatShape(FatShape),
    /// See [`FatText`].
    FatText(FatText),
}

/// Bag of [`GraphicsItem`]s.
#[derive(Debug)]
pub struct GraphicsBag {
    /// [`GraphicsItem`]s in the bag.
    pub items: Vec<GraphicsItem>,
    /// Fully realized transforms used for rendering.
    final_transforms: Vec<Affine>,
    /// Records that
    managed_transforms: Vec<ManagedTransform>,
    /// `FatPaint`s registered with this bag.
    palette: Vec<FatPaint>,
}

impl Default for GraphicsBag {
    fn default() -> Self {
        Self {
            // Always initialize with a root transform.
            final_transforms: vec![Default::default()],
            managed_transforms: vec![Default::default()],
            items: Default::default(),
            palette: Default::default(),
        }
    }
}

impl GraphicsBag {
    /// Push a [`GraphicsItem`], returning its index.
    pub fn push(&mut self, i: impl Into<GraphicsItem>) -> ItemHandle {
        let n = self.items.len();
        if n >= u32::MAX as usize {
            panic!("GraphicsBag has too many items.");
        }
        self.items.push(i.into());
        ItemHandle(n.try_into().unwrap())
    }

    /// Get an individual [`GraphicsItem`].
    #[must_use]
    pub fn get(&self, idx: ItemHandle) -> &GraphicsItem {
        &self.items[idx.0 as usize]
    }

    /// Get an individual [`GraphicsItem`].
    #[must_use]
    pub fn get_mut(&mut self, idx: ItemHandle) -> &mut GraphicsItem {
        &mut self.items[idx.0 as usize]
    }

    /// Register a paint.
    ///
    /// Attach the returned `PaintHandle` to a `GraphicsItem`.
    #[must_use]
    pub fn register_paint(&mut self, paint: FatPaint) -> PaintHandle {
        let n = self.palette.len();
        if n >= u32::MAX as usize {
            panic!("GraphicsBag has too many paints.");
        }
        self.palette.push(paint);
        PaintHandle(n.try_into().unwrap())
    }

    /// Get a paint.
    #[must_use]
    pub fn get_paint(&self, handle: PaintHandle) -> &FatPaint {
        self.palette.get(usize::from(handle)).unwrap()
    }

    /// Get a paint.
    #[must_use]
    pub fn get_paint_mut(&mut self, handle: PaintHandle) -> &mut FatPaint {
        self.palette.get_mut(usize::from(handle)).unwrap()
    }

    /// Update a paint.
    pub fn update_paint(&mut self, handle: PaintHandle, paint: FatPaint) {
        self.palette[handle.0 as usize] = paint;
    }

    /// Register a transform.
    ///
    /// Attach the returned `TransformHandle` to a `GraphicsItem`.
    pub fn register_transform(
        &mut self,
        parent: TransformHandle,
        local: Affine,
    ) -> TransformHandle {
        #[allow(
            clippy::cast_possible_truncation,
            reason = "The length of managed_transforms is managed."
        )]
        let handle = TransformHandle(NonZeroU32::new(self.managed_transforms.len() as u32));
        let managed = ManagedTransform { parent, local };

        self.managed_transforms.push(managed);

        self.final_transforms
            .push(self.final_transforms[usize::from(parent)] * local);

        handle
    }

    /// Get a transform.
    pub fn get_transform(&self, handle: TransformHandle) -> Affine {
        *self.final_transforms.get(usize::from(handle)).unwrap()
    }

    /// Update a transform.
    pub fn update_transform(&mut self, handle: TransformHandle, local: Affine) {
        self.managed_transforms[usize::from(handle)].local = local;
        self.finalize_transforms(handle);
    }

    // TODO: Consider finalizing transforms based on a dirty state immediately
    //       before rendering or picking.
    /// Update a set of transforms by pairs of `TransformHandle` and local `Affine`.
    #[tracing::instrument(skip_all)]
    pub fn update_transforms(
        &mut self,
        pairs: impl IntoIterator<Item = (TransformHandle, Affine)>,
    ) {
        let mut includes_root = false;
        let mut least = NonZeroU32::MAX;
        for (k, v) in pairs {
            self.managed_transforms[usize::from(k)].local = v;

            if let Some(i) = k.0 {
                least = least.min(i);
            } else {
                includes_root = true;
            }
        }

        // Empty iterator, do nothing.
        if least == NonZeroU32::MAX {
            return;
        }

        self.finalize_transforms(if includes_root {
            Default::default()
        } else {
            TransformHandle(Some(least))
        });
    }

    /// Finalize all transforms that may depend on `handle`.
    fn finalize_transforms(&mut self, handle: TransformHandle) {
        for i in usize::from(handle)..self.managed_transforms.len() {
            let ManagedTransform { parent, local } = self.managed_transforms[i];
            // Special case for root transform.
            self.final_transforms[i] = if i == 0 {
                local
            } else {
                self.final_transforms[usize::from(parent)] * local
            }
        }
    }
}
