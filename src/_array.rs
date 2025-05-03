use core::{
    borrow::{Borrow, BorrowMut},
    cmp,
    ffi::{CStr, c_char},
    fmt,
    hash::{Hash, Hasher},
    mem, ops, slice,
};

/// A fixed-sized array that may be truncated by an interior null.
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
        let len = self.bytes().len();
        let chars: &mut [c_char] = &mut self.0;
        unsafe { slice::from_raw_parts_mut(chars.as_mut_ptr().cast::<u8>(), len) }
    }
    /// Return the entire inner array, as bytes.
    pub const fn all_bytes(&self) -> &[u8; N] {
        unsafe { mem::transmute_copy(&self.0) }
    }
    /// Return the entire inner array, as bytes.
    pub const fn all_bytes_mut(&mut self) -> &mut [u8; N] {
        unsafe { mem::transmute(&mut self.0) }
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
