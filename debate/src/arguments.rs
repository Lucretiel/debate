use std::{
    ffi::{OsStr, OsString},
    vec::Vec,
};

use debate_parser::ArgumentsParser;

use crate::{
    build,
    from_args::{self, FromArgs},
};

#[derive(Debug, Clone)]
pub struct LoadedArguments {
    arguments: Vec<OsString>,
}

impl LoadedArguments {
    pub fn from_env() -> Self {
        Self {
            arguments: std::env::args_os().collect(),
        }
    }

    pub fn argv0(&self) -> &OsStr {
        self.arguments[0].as_os_str()
    }

    pub fn parser(&self) -> ArgumentsParser<'_, impl Iterator<Item = &[u8]>> {
        ArgumentsParser::new(
            self.arguments
                .get(1..)
                .unwrap_or(&[])
                .iter()
                .map(|arg| arg.as_encoded_bytes()),
        )
    }

    pub fn try_parse<'a, T, E>(&'a self) -> Result<T, E>
    where
        T: FromArgs<'a>,
        E: from_args::Error<'a> + build::Error,
    {
        T::try_from_parser(self.parser())
    }

    pub fn parse<'a, T>(&'a self) -> T
    where
        T: FromArgs<'a>,
    {
        todo!()
    }
}
