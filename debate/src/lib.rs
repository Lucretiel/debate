#![no_std]
#![feature(try_trait_v2)]

#[cfg(feature = "std")]
extern crate std;

pub mod build;
pub mod errors;
pub mod from_args;
pub mod help;
mod impls;
pub mod parameter;
pub mod state;
pub mod util;

#[cfg(feature = "std")]
pub mod arguments;

#[cfg(feature = "std")]
mod printers;

// TODO: re-exports
