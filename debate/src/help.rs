#[derive(Debug, Clone, Copy)]
pub enum Requirement {
    Optional,
    Mandatory,
}

#[derive(Debug, Clone, Copy)]
pub enum Repetition {
    Single,
    Multiple,
}

pub trait PositionalReceiver {
    fn argument<'a>(
        &mut self,
        name: &str,
        requirement: Requirement,
        repetition: Repetition,
        short_description: &str,
        long_description_paragraphs: &[&str],
        values: Option<impl Iterator<Item = &'a str>>,
    );
}

pub trait OptionReceiver {
    fn option<'a>(
        &mut self,
        tags: Tags<'_>,
        requirement: Requirement,
        repetition: Repetition,
        short_description: &str,
        long_description_paragraphs: &[&str],
        argument: Option<Argument<'a, impl Iterator<Item = &'a str>>>,
    );
}

#[derive(Debug)]
pub struct Argument<'a, I: Iterator<Item = &'a str>> {
    /// The name of the argument itself. Typically capitalized.
    pub name: &'a str,

    /// If given, the set of possible values that this argument can be.
    pub values: Option<I>,
}

#[derive(Debug, Clone, Copy)]
pub enum Tags<'a> {
    Long { long: &'a str },
    Short { short: char },
    LongShort { long: &'a str, short: char },
}

impl<'a> Tags<'a> {
    pub fn long(&self) -> Option<&'a str> {
        match self {
            Tags::Long { long } | Tags::LongShort { long, .. } => Some(long),
            Tags::Short { .. } => None,
        }
    }

    pub fn short(&self) -> Option<char> {
        match self {
            Tags::Short { short } | Tags::LongShort { short, .. } => Some(*short),
            Tags::Long { .. } => None,
        }
    }
}

pub trait Usage {
    fn describe_options(receiver: impl OptionReceiver);
    fn describe_positionals(receiver: impl PositionalReceiver);
}

pub trait ParameterUsage {
    fn requirement() -> Requirement;
    fn repetition() -> Repetition;
    fn possible_values() -> Option<impl Iterator<Item = &'static str>>;
}
