use core::{hint, mem};

/// A slice that is statically guaranteed to have a length of at least 1
#[derive(Debug)]
#[repr(transparent)]
pub struct PopulatedSlice<T>([T]);

impl<T> PopulatedSlice<T> {
    /// SAFETY: the slice's length must be greater than 0
    #[inline(always)]
    pub unsafe fn new_unchecked(slice: &[T]) -> &Self {
        debug_assert!(!slice.is_empty());

        unsafe { mem::transmute(slice) }
    }

    #[inline]
    pub fn new(slice: &[T]) -> Option<&Self> {
        match slice.is_empty() {
            true => None,
            // Safety: we just confirmed that the length is greater than 0
            false => Some(unsafe { Self::new_unchecked(slice) }),
        }
    }

    /// The whole point: a static guarantee that this slice is not empty
    #[inline]
    pub const fn split_first(&self) -> (&T, &[T]) {
        debug_assert!(!self.0.is_empty());

        match self.0 {
            [ref head, ref tail @ ..] => (head, tail),

            // Safety: `self.0` is guaranteed to be non-empty, so the split is
            // guaranteed to exist
            _ => unsafe { hint::unreachable_unchecked() },
        }
    }

    #[inline(always)]
    pub fn get(&self) -> &[T] {
        &self.0
    }
}
