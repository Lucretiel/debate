use crate::parameter;

pub enum DetectUnrecognized<A, E> {
    Unrecognized(A),
    Error(E),
}

/// When used as a parameter error, we treat `got_additional_instance`
/// as an unrecognized parameter
impl<'arg, E> parameter::Error<'arg> for DetectUnrecognized<(), E>
where
    E: parameter::Error<'arg>,
{
    fn needs_arg() -> Self {
        Self::Error(E::needs_arg())
    }

    fn got_arg(arg: debate_parser::Arg<'arg>) -> Self {
        Self::Error(E::got_arg(arg))
    }

    fn got_additional_instance() -> Self {
        Self::Unrecognized(())
    }

    fn invalid_utf8(arg: debate_parser::Arg<'arg>) -> Self {
        Self::Error(E::invalid_utf8(arg))
    }

    fn parse_error(arg: &str, msg: impl core::fmt::Display) -> Self {
        Self::Error(E::parse_error(arg, msg))
    }

    fn byte_parse_error(arg: debate_parser::Arg<'arg>, msg: impl core::fmt::Display) -> Self {
        Self::Error(E::byte_parse_error(arg, msg))
    }

    fn custom(msg: impl core::fmt::Display) -> Self {
        Self::Error(E::custom(msg))
    }
}
