use crate::from_args;

pub enum DetectUnrecognized<A, E> {
    Unrecognized(A),
    Error(E),
}

impl<'arg, A, E> from_args::StateError<'arg, A> for DetectUnrecognized<A, E>
where
    E: from_args::StateError<'arg, A>,
{
    type ParameterError = E::ParameterError;

    fn parameter(field: &'static str, error: Self::ParameterError) -> Self {
        Self::Error(E::parameter(field, error))
    }

    fn unrecognized(argument: A) -> Self {
        Self::Unrecognized(argument)
    }

    fn rejected() -> Self {
        Self::Error(E::rejected())
    }
}
