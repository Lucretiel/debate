use core::{fmt, mem};

/**
A single, raw argument passed in from the command line.

This type is used in two ways: to indicate long command line options, and to
indicate arguments themselves. For instance, given
`--target foo --path=bar input.txt`, `target`, `foo`, `path`, `bar`, and
`input.txt` would all be passed as [`Arg`] values to the relevant functions.

An [`Arg`] internally is just a byte slice, since that's what the OS gives us.
Callers can manually turn it into a [`str`] with [`from_utf8`][core::str::from_utf8],
and from there parse it however they need.
*/
#[derive(Eq, PartialEq, Hash)]
#[repr(transparent)]
pub struct Arg([u8]);

impl Arg {
    pub const fn new(bytes: &[u8]) -> &Self {
        // SAFETY: Arg is repr transparent to a byte slice, so it's safe to
        // transmute into it.
        unsafe { mem::transmute(bytes) }
    }

    pub const fn bytes(&self) -> &[u8] {
        &self.0
    }
}

impl PartialEq<[u8]> for Arg {
    fn eq(&self, other: &[u8]) -> bool {
        self.0 == *other
    }
}

impl PartialEq<&[u8]> for Arg {
    fn eq(&self, other: &&[u8]) -> bool {
        self.0 == **other
    }
}

impl PartialEq<str> for Arg {
    fn eq(&self, other: &str) -> bool {
        self.0 == *other.as_bytes()
    }
}

impl fmt::Debug for Arg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match str::from_utf8(self.bytes()) {
            Ok(s) => write!(f, "{:?}", s),
            Err(_) => write!(f, "{:?}", &self.0),
        }
    }
}
