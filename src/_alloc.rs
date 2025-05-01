use core::{alloc::Layout, fmt, ptr::NonNull};

#[cfg(feature = "libc")]
use core::{ffi::c_void, mem, ptr};

/// A set of free allocation functions.
///
/// # Safety
/// - Must act like an allocator ;)
pub unsafe trait Allocator {
    /// Whether an empty layout returns [`None`] or [`Some`] is implementation-dependent,
    /// and users interested in this case should handle that before this call.
    fn alloc(layout: Layout) -> Option<NonNull<u8>>;
    /// # Safety
    /// - `ptr` must have been from a call to [`Allocator::alloc`].
    unsafe fn free(ptr: NonNull<u8>);
}

/// An [`Allocator`] which uses [`libc`]'s allocation functions.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg(feature = "libc")]
#[cfg_attr(docsrs, doc(cfg(feature = "libc")))]
pub struct Libc;

#[cfg(feature = "libc")]
#[cfg_attr(docsrs, doc(cfg(feature = "libc")))]
unsafe impl Allocator for Libc {
    fn alloc(layout: Layout) -> Option<NonNull<u8>> {
        NonNull::new(
            match layout.align() == 1 {
                true => unsafe { libc::malloc(layout.size()) },
                false => {
                    let mut ptr = ptr::null_mut();
                    let layout = layout.align_to(mem::size_of::<c_void>()).ok()?;
                    unsafe { libc::posix_memalign(&mut ptr, layout.align(), layout.size()) };
                    ptr
                }
            }
            .cast::<u8>(),
        )
    }
    unsafe fn free(ptr: NonNull<u8>) {
        unsafe { libc::free(ptr.as_ptr().cast()) }
    }
}

/// Heap allocation failure.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AllocError(pub usize);

impl AllocError {
    pub fn into_layout(self) -> Layout {
        Layout::array::<u8>(self.0).unwrap()
    }
    #[cfg(feature = "alloc")]
    #[cfg_attr(docsrs, doc(cfg(feature = "alloc")))]
    pub fn handle(self) -> ! {
        alloc::alloc::handle_alloc_error(self.into_layout())
    }
}

impl fmt::Display for AllocError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("failed to allocate {} bytes", self.0))
    }
}

#[cfg(not(feature = "std"))]
impl core::error::Error for AllocError {}

#[cfg(feature = "std")]
impl std::error::Error for AllocError {}

#[cfg(feature = "std")]
impl From<AllocError> for std::io::ErrorKind {
    fn from(_: AllocError) -> Self {
        std::io::ErrorKind::OutOfMemory
    }
}
#[cfg(feature = "std")]
impl From<AllocError> for std::io::Error {
    fn from(value: AllocError) -> Self {
        fn try_box<T>(x: T) -> Option<std::boxed::Box<T>> {
            let layout = Layout::for_value(&x);
            assert_ne!(layout.size(), 0);
            // SAFETY:
            // - checked size != 0 above
            let ptr = unsafe { std::alloc::alloc(layout) };
            match ptr.is_null() {
                true => None,
                // SAFETY:
                // - this is called out as safe in the `Box` docs
                false => Some(unsafe { std::boxed::Box::from_raw(ptr.cast()) }),
            }
        }

        // try and preserve the source
        match try_box(value) {
            Some(src) => std::io::Error::new(
                std::io::ErrorKind::OutOfMemory,
                src as std::boxed::Box<dyn std::error::Error + Send + Sync>,
            ),
            None => std::io::Error::from(std::io::ErrorKind::from(value)),
        }
    }
}
