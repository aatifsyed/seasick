use alloc::{
    alloc::handle_alloc_error,
    borrow::Cow,
    collections::TryReserveError,
    ffi::{CString, IntoStringError, NulError},
    string::String,
};
use core::{
    alloc::Layout,
    borrow::Borrow,
    ffi::CStr,
    fmt,
    str::{self, FromStr},
};

/// A growable, nul-terminated UTF-8 buffer which implements [`fmt::Write`].
///
/// Any `nul`s that are [written](fmt::Write::write_fmt) to this are replaced with [`char::REPLACEMENT_CHARACTER`].
/// Allocation errors are exposed in [`fmt::write`]s.
#[derive(Default, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(docsrs, doc(cfg(feature = "alloc")))]
pub struct WriteBuffer {
    buffer: String,
}

impl WriteBuffer {
    const REPLACEMENT_STR: &str = "�";
    const TERMINATION_STR: &str = "\0";
    pub const fn new() -> Self {
        Self {
            buffer: String::new(),
        }
    }
    pub fn with_capacity(cap: usize) -> Self {
        Self {
            buffer: String::with_capacity(cap),
        }
    }
    /// Not including the nul terminator.
    pub fn as_str(&self) -> &str {
        match self.buffer.as_bytes().split_last() {
            Some((last, rest)) => {
                debug_assert_eq!(*last, 0);
                match cfg!(debug_assertions) {
                    true => str::from_utf8(rest).unwrap(),
                    false => unsafe { str::from_utf8_unchecked(rest) },
                }
            }
            None => "",
        }
    }
    pub fn as_cstr(&self) -> &CStr {
        if self.buffer.is_empty() {
            return c"";
        }
        let bytes = self.buffer.as_bytes();
        match cfg!(debug_assertions) {
            true => CStr::from_bytes_with_nul(bytes).unwrap(),
            false => unsafe { CStr::from_bytes_with_nul_unchecked(bytes) },
        }
    }
    pub fn clear(&mut self) {
        // Safety:
        // - clearing is safe
        unsafe { self.buffer_mut() }.clear();
    }
    pub fn try_reserve(&mut self, additional: usize) -> Result<(), TryReserveError> {
        // Safety:
        // - reservation does not modify string
        unsafe { self.buffer_mut() }.try_reserve(additional)
    }
    /// # Safety
    /// Returned string must be either made [`empty`](String::is_empty),
    /// or be nul-terminated.
    unsafe fn buffer_mut(&mut self) -> &mut String {
        &mut self.buffer
    }
    /// Reserves enough space for `s` and [`Self::TERMINATION_STR`],
    /// before pushing `s`.
    ///
    /// On error, the contents are unmodified.
    ///
    /// # Safety
    /// - Must terminate buffer after calling this method.
    unsafe fn try_push_str(&mut self, s: &str) -> fmt::Result {
        let Some(len) = s.len().checked_add(Self::TERMINATION_STR.len()) else {
            return Err(fmt::Error);
        };
        // Safety:
        // - reservation does not change string contents
        match self.try_reserve(len) {
            Ok(()) => {
                // Safety:
                // - Caller promises to terminate string
                unsafe { self.buffer_mut() }.push_str(s);
                Ok(())
            }
            Err(_) => Err(fmt::Error),
        }
    }
    fn terminate(&mut self) {
        unsafe { self.buffer_mut() }.push_str(Self::TERMINATION_STR);
    }
}

impl fmt::Write for WriteBuffer {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        // Safety:
        // - we'll add ther terminator back before returning.
        unsafe { self.buffer_mut() }.pop();
        let wrote = (|| {
            let mut spans = s.split('\0').peekable();
            while let Some(span) = spans.next() {
                // Safety:
                // - We always terminate before returning from this function.
                unsafe {
                    self.try_push_str(span)?;
                    if spans.peek().is_some() {
                        self.try_push_str(Self::REPLACEMENT_STR)?
                    }
                }
            }
            Ok(())
        })();
        match wrote {
            Ok(()) => {
                self.terminate();
                Ok(())
            }
            Err(fmt::Error) => {
                unsafe { self.buffer_mut().pop() };
                self.terminate();
                Err(fmt::Error)
            }
        }
    }
}

impl fmt::Display for WriteBuffer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_str().fmt(f)
    }
}
/// Will allocate
impl FromStr for WriteBuffer {
    type Err = NulError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.is_empty() {
            return Ok(WriteBuffer::new());
        }
        Ok(CString::new(s)?.try_into().unwrap())
    }
}

impl From<&str> for WriteBuffer {
    fn from(value: &str) -> Self {
        let mut this = Self::new();
        match fmt::write(&mut this, format_args!("{}", value)) {
            Ok(()) => this,
            Err(fmt::Error) => handle_alloc_error(Layout::for_value(value)),
        }
    }
}

/// Does not allocate
impl From<WriteBuffer> for Cow<'static, CStr> {
    fn from(value: WriteBuffer) -> Self {
        match value.buffer.is_empty() {
            true => Cow::Borrowed(c""),
            false => Cow::Owned(match cfg!(debug_assertions) {
                true => CString::from_vec_with_nul(value.buffer.into()).unwrap(),
                false => unsafe { CString::from_vec_with_nul_unchecked(value.buffer.into()) },
            }),
        }
    }
}
/// Does not allocate
impl From<WriteBuffer> for String {
    fn from(value: WriteBuffer) -> Self {
        let mut buffer = value.buffer;
        buffer.pop();
        buffer
    }
}
/// May allocate
impl From<WriteBuffer> for CString {
    fn from(value: WriteBuffer) -> Self {
        Cow::from(value).into_owned()
    }
}
/// Does not allocate
impl TryFrom<CString> for WriteBuffer {
    type Error = IntoStringError;
    fn try_from(value: CString) -> Result<Self, Self::Error> {
        let mut buffer = CString::into_string(value)?;
        buffer.push_str(Self::TERMINATION_STR);
        Ok(Self { buffer })
    }
}

impl AsRef<str> for WriteBuffer {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}
impl AsRef<CStr> for WriteBuffer {
    fn as_ref(&self) -> &CStr {
        self.as_cstr()
    }
}
impl Borrow<str> for WriteBuffer {
    fn borrow(&self) -> &str {
        self.as_str()
    }
}
impl Borrow<CStr> for WriteBuffer {
    fn borrow(&self) -> &CStr {
        self.as_cstr()
    }
}

impl PartialEq<str> for WriteBuffer {
    fn eq(&self, other: &str) -> bool {
        self.as_str() == other
    }
}
impl PartialEq<WriteBuffer> for str {
    fn eq(&self, other: &WriteBuffer) -> bool {
        other == self
    }
}
impl PartialEq<CStr> for WriteBuffer {
    fn eq(&self, other: &CStr) -> bool {
        self.as_cstr() == other
    }
}
impl PartialEq<WriteBuffer> for CStr {
    fn eq(&self, other: &WriteBuffer) -> bool {
        other == self
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test() {
        let mut buf = WriteBuffer::from("hello");
        assert_eq!(&buf, "hello");
        assert_eq!(&buf, c"hello");
        fmt::write(&mut buf, format_args!("\0world!")).unwrap();
        assert_eq!(&buf, "hello�world!");
        assert_eq!(&buf, c"hello�world!");
    }
}
