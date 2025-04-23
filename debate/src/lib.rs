#![no_std]

#[cfg(feature = "std")]
extern crate std;

use std::ffi::OsString;

use debate_parser::ArgumentsParser;
use from_args::FromArgs;

pub mod build;
pub mod errors;
pub mod from_args;
pub mod help;
mod impls;
pub mod parameter;
pub mod printers;
pub mod state;
pub mod util;

#[cfg(feature = "std")]
pub mod arguments;
