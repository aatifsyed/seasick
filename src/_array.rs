use core::{
    borrow::{Borrow, BorrowMut},
    cmp,
    ffi::{CStr, c_char},
    fmt,
    hash::{Hash, Hasher},
    ops, ptr, slice,
};

/// A fixed-sized array that MAY be truncated by an interior null.
#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct SeaArray<const N: usize>(pub [c_char; N]);

impl<const N: usize> fmt::Display for SeaArray<N> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        crate::display_bytes(self.bytes(), f)
    }
}

impl<const N: usize> fmt::Debug for SeaArray<N> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        crate::debug_bytes(self.bytes(), f)
    }
}

impl<const N: usize> SeaArray<N> {
    pub const fn new(a: [c_char; N]) -> Self {
        Self(a)
    }
    pub const fn new_ref(a: &[c_char; N]) -> &Self {
        let ptr = a as *const [c_char; N] as *const Self;
        unsafe { &*ptr }
    }
    pub const fn new_mut(a: &mut [c_char; N]) -> &mut Self {
        let ptr = a as *mut [c_char; N] as *mut Self;
        unsafe { &mut *ptr }
    }
    /// Return up to the nul-terminator, or the entire contents.
    pub const fn bytes(&self) -> &[u8] {
        let bytes = self.all_bytes();
        match CStr::from_bytes_until_nul(bytes) {
            Ok(cstr) => cstr.to_bytes(),
            Err(_) => bytes,
        }
    }
    /// Return up to the nul-terminator, or the entire contents.
    pub const fn bytes_mut(&mut self) -> &mut [u8] {
        let ptr = self.all_bytes_mut().as_mut_ptr();
        let len = self.bytes().len();
        unsafe { slice::from_raw_parts_mut(ptr, len) }
    }
    /// Return the entire inner array, as bytes.
    pub const fn all_bytes(&self) -> &[u8; N] {
        let ptr = self as *const Self as *const [u8; N];
        unsafe { &*ptr }
    }
    /// Return the entire inner array, as bytes.
    pub const fn all_bytes_mut(&mut self) -> &mut [u8; N] {
        let ptr = self as *mut Self as *mut [u8; N];
        unsafe { &mut *ptr }
    }
    /// Set the contents of the array to `src`.
    ///
    /// Any spare space will be zeroed.
    ///
    /// # Panics
    /// - If `src` does not fit.
    pub const fn set(&mut self, src: &[u8]) {
        match self.all_bytes_mut().split_at_mut_checked(src.len()) {
            Some((dst, end)) => {
                debug_assert!(src.len() == dst.len());
                unsafe { ptr::copy_nonoverlapping(src.as_ptr(), dst.as_mut_ptr(), src.len()) }
                let mut ix = 0;
                while ix < end.len() {
                    end[ix] = 0;
                    ix += 1
                }
            }
            None => panic!("`src` too big to fit in SeaArray"),
        }
    }
}

impl<const N: usize> PartialEq for SeaArray<N> {
    fn eq(&self, other: &Self) -> bool {
        self.bytes() == other.bytes()
    }
}
impl<const N: usize> Eq for SeaArray<N> {}
impl<const N: usize> Hash for SeaArray<N> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.bytes().hash(state);
    }
}
impl<const N: usize> Ord for SeaArray<N> {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.bytes().cmp(other.bytes())
    }
}
impl<const N: usize> PartialOrd for SeaArray<N> {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl<const N: usize> AsRef<[u8]> for SeaArray<N> {
    fn as_ref(&self) -> &[u8] {
        self.bytes()
    }
}
impl<const N: usize> Borrow<[u8]> for SeaArray<N> {
    fn borrow(&self) -> &[u8] {
        self.bytes()
    }
}
impl<const N: usize> AsMut<[u8]> for SeaArray<N> {
    fn as_mut(&mut self) -> &mut [u8] {
        self.bytes_mut()
    }
}
impl<const N: usize> BorrowMut<[u8]> for SeaArray<N> {
    fn borrow_mut(&mut self) -> &mut [u8] {
        self.bytes_mut()
    }
}

impl<const N: usize> ops::Deref for SeaArray<N> {
    type Target = [u8];
    fn deref(&self) -> &Self::Target {
        self.bytes()
    }
}
impl<const N: usize> ops::DerefMut for SeaArray<N> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.bytes_mut()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test() {
        let mut arr = SeaArray([0; 64]);
        assert_eq!(arr.len(), 0);

        let Some(prefix) = arr.all_bytes_mut().first_chunk_mut() else {
            unreachable!()
        };
        *prefix = *b"hello";

        assert_eq!(&*arr, b"hello");
    }

    #[test]
    fn set() {
        let mut arr = SeaArray([1; 7]);
        arr.set(b"hello");
        assert_eq!(&*arr, b"hello");
        assert_eq!(arr.all_bytes(), b"hello\0\0");
        arr.set(b"goodbye");
        assert_eq!(&*arr, b"goodbye");
        assert_eq!(arr.all_bytes(), b"goodbye");
    }

    #[test]
    #[should_panic = "`src` too big to fit in SeaArray"]
    fn set2big() {
        let mut arr = SeaArray([0; 7]);
        arr.set(b"goodbye!");
    }
}
