use std::path::PathBuf;
use std::str;

use anyhow::Context;
use debate::{
    from_args::{self, BuildFromArgs, FromArgs},
    parameter::{self, Parameter},
    state::{self, State},
};
use debate_derive::{FromArgs, Value};
use debate_parser::{Arg, ArgumentsParser};

// Recursive expansion of FromArgs macro
// ======================================

#[doc(hidden)]
struct __ArgumentsState<'arg>{
    position:u16,phantom: ::core::marker::PhantomData< &'arg()> ,fields:(::core::option::Option<PathBuf> , ::core::option::Option<bool> , ::core::option::Option<Option<PathBuf> > , ::core::option::Option<i32> , <Alphabet as ::debate::from_args::BuildFromArgs<'arg>> ::State, ::core::option::Option<String> , ::core::option::Option<Vec<String> > ,),
}
impl <'arg> ::core::default::Default for __ArgumentsState<'arg>{
    fn default() -> Self {
        Self {
            position:0,phantom: ::core::marker::PhantomData,fields:(::core::option::Option::None, ::core::option::Option::None, ::core::option::Option::None, ::core::option::Option::None, ::core::default::Default::default(), ::core::option::Option::None, ::core::option::Option::None,),
        }
    }

    }
impl <'arg> ::debate::state::State<'arg>for __ArgumentsState<'arg>{
    fn add_positional<E>(&mut self,argument: ::debate_parser::Arg<'arg>) ->  ::core::result::Result<(),E>where E: ::debate::state::Error<'arg,()>{
        let fields =  &mut self.fields;
        let position =  &mut self.position;
        if*position==0 {
            *position = match(::debate::state::State::add_positional(&mut fields.4,argument)){
                ::core::result::Result::Ok(()) => return::core::result::Result::Ok(()),
                ::core::result::Result::Err(err) => match err {
                    ::debate::util::DetectUnrecognized::Unrecognized(()) => 0+1,
                    ::debate::util::DetectUnrecognized::Error(error) => return::debate::state::Error::flattened("inner",error)
                    ;

                    }

                };
        }if*position==1 {
            *position = match(match fields.5 {
                ::core::option::Option::None => match::debate::parameter::Parameter::arg(argument){
                    ::core::result::Result::Err(err) =>  ::core::result::Result::Err(err),
                    ::core::result::Result::Ok(value) => {
                        fields.5 =  ::core::option::Option::Some(value);
                        ::core::result::Result::Ok(())
                    }

                    }
                ::core::option::Option::Some(ref mut old) =>  ::debate::parameter::Parameter::add_arg(old,argument),

                }){
                ::core::result::Result::Ok(()) => return::core::result::Result::Ok(()),
                ::core::result::Result::Err(err) => match err {
                    ::debate::util::DetectUnrecognized::Unrecognized(()) => 1+1,
                    ::debate::util::DetectUnrecognized::Error(error) => return::debate::state::Error::parameter("input",error)
                    ;

                    }

                };
        }if*position==2 {
            *position = match(match fields.6 {
                ::core::option::Option::None => match::debate::parameter::Parameter::arg(argument){
                    ::core::result::Result::Err(err) =>  ::core::result::Result::Err(err),
                    ::core::result::Result::Ok(value) => {
                        fields.6 =  ::core::option::Option::Some(value);
                        ::core::result::Result::Ok(())
                    }

                    }
                ::core::option::Option::Some(ref mut old) =>  ::debate::parameter::Parameter::add_arg(old,argument),

                }){
                ::core::result::Result::Ok(()) => return::core::result::Result::Ok(()),
                ::core::result::Result::Err(err) => match err {
                    ::debate::util::DetectUnrecognized::Unrecognized(()) => 2+1,
                    ::debate::util::DetectUnrecognized::Error(error) => return::debate::state::Error::parameter("other_inputs",error)
                    ;

                    }

                };
        }::core::result::Result::Err(::debate::state::Error::unrecognized(()))
    }
    fn add_long_option<E>(&mut self,option: ::debate_parser::Arg<'arg> ,argument: ::debate_parser::Arg<'arg>) ->  ::core::result::Result<(),E>where E: ::debate::state::Error<'arg,()>{
        let fields =  &mut self.fields;
        let position =  &mut self.position;
        match option.bytes(){
            b"foo" => match(match fields.0 {
                ::core::option::Option::None => match::debate::parameter::Parameter::arg(argument){
                    ::core::result::Result::Err(err) =>  ::core::result::Result::Err(err),
                    ::core::result::Result::Ok(value) => {
                        fields.0 =  ::core::option::Option::Some(value);
                        ::core::result::Result::Ok(())
                    }

                    }
                ::core::option::Option::Some(ref mut old) =>  ::debate::parameter::Parameter::add_arg(old,argument),

                }){
                ::core::result::Result::Ok(()) =>  ::core::result::Result::Ok(()),
                ::core::result::Result::Err(err) =>  ::core::result::Result::Err(::debate::state::Error::parameter("path",err)),

                },
            b"verbose" => match(match fields.1 {
                ::core::option::Option::None => match::debate::parameter::Parameter::arg(argument){
                    ::core::result::Result::Err(err) =>  ::core::result::Result::Err(err),
                    ::core::result::Result::Ok(value) => {
                        fields.1 =  ::core::option::Option::Some(value);
                        ::core::result::Result::Ok(())
                    }

                    }
                ::core::option::Option::Some(ref mut old) =>  ::debate::parameter::Parameter::add_arg(old,argument),

                }){
                ::core::result::Result::Ok(()) =>  ::core::result::Result::Ok(()),
                ::core::result::Result::Err(err) =>  ::core::result::Result::Err(::debate::state::Error::parameter("verbose",err)),

                },
            b"cool-value" => match(match fields.3 {
                ::core::option::Option::None => match::debate::parameter::Parameter::arg(argument){
                    ::core::result::Result::Err(err) =>  ::core::result::Result::Err(err),
                    ::core::result::Result::Ok(value) => {
                        fields.3 =  ::core::option::Option::Some(value);
                        ::core::result::Result::Ok(())
                    }

                    }
                ::core::option::Option::Some(ref mut old) =>  ::debate::parameter::Parameter::add_arg(old,argument),

                }){
                ::core::result::Result::Ok(()) =>  ::core::result::Result::Ok(()),
                ::core::result::Result::Err(err) =>  ::core::result::Result::Err(::debate::state::Error::parameter("value",err)),

                },
            _ => {
                match(::debate::state::State::add_long_option(&mut fields.4,option,argument)){
                    ::core::result::Result::Ok(()) => return::core::result::Result::Ok(()),
                    ::core::result::Result::Err(err) => match err {
                        ::debate::util::DetectUnrecognized::Unrecognized(()) => {}
                        ,
                        ::debate::util::DetectUnrecognized::Error(error) => return::debate::state::Error::flattened("inner",error)
                        ;

                        }

                    }::core::result::Result::Err(::debate::state::Error::unrecognized(()))
            }

            }
    }
    fn add_long<A,E>(&mut self,option: ::debate_parser::Arg<'arg> ,argument:A) ->  ::core::result::Result<(),E>where A: ::debate_parser::ArgAccess<'arg> ,E: ::debate::state::Error<'arg,A>{
        let fields =  &mut self.fields;
        let position =  &mut self.position;
        match option.bytes(){
            b"foo" => match(match fields.0 {
                ::core::option::Option::None => match::debate::parameter::Parameter::present(argument){
                    ::core::result::Result::Err(err) =>  ::core::result::Result::Err(err),
                    ::core::result::Result::Ok(value) => {
                        fields.0 =  ::core::option::Option::Some(value);
                        ::core::result::Result::Ok(())
                    }

                    }
                ::core::option::Option::Some(ref mut old) =>  ::debate::parameter::Parameter::add_present(old,argument),

                }){
                ::core::result::Result::Ok(()) =>  ::core::result::Result::Ok(()),
                ::core::result::Result::Err(err) =>  ::core::result::Result::Err(::debate::state::Error::parameter("path",err)),

                },
            b"verbose" => match(match fields.1 {
                ::core::option::Option::None => match::debate::parameter::Parameter::present(argument){
                    ::core::result::Result::Err(err) =>  ::core::result::Result::Err(err),
                    ::core::result::Result::Ok(value) => {
                        fields.1 =  ::core::option::Option::Some(value);
                        ::core::result::Result::Ok(())
                    }

                    }
                ::core::option::Option::Some(ref mut old) =>  ::debate::parameter::Parameter::add_present(old,argument),

                }){
                ::core::result::Result::Ok(()) =>  ::core::result::Result::Ok(()),
                ::core::result::Result::Err(err) =>  ::core::result::Result::Err(::debate::state::Error::parameter("verbose",err)),

                },
            b"cool-value" => match(match fields.3 {
                ::core::option::Option::None => match::debate::parameter::Parameter::present(argument){
                    ::core::result::Result::Err(err) =>  ::core::result::Result::Err(err),
                    ::core::result::Result::Ok(value) => {
                        fields.3 =  ::core::option::Option::Some(value);
                        ::core::result::Result::Ok(())
                    }

                    }
                ::core::option::Option::Some(ref mut old) =>  ::debate::parameter::Parameter::add_present(old,argument),

                }){
                ::core::result::Result::Ok(()) =>  ::core::result::Result::Ok(()),
                ::core::result::Result::Err(err) =>  ::core::result::Result::Err(::debate::state::Error::parameter("value",err)),

                },
            _ => {
                let argument = match(::debate::state::State::add_long(&mut fields.4,option,argument)){
                    ::core::result::Result::Ok(()) => return::core::result::Result::Ok(()),
                    ::core::result::Result::Err(err) => match err {
                        ::debate::util::DetectUnrecognized::Unrecognized(argument) => argument,
                        ::debate::util::DetectUnrecognized::Error(error) => return::debate::state::Error::flattened("inner",error)
                        ;

                        }

                    };
                ::core::result::Result::Err(::debate::state::Error::unrecognized(argument))
            }

            }
    }
    fn add_short<A,E>(&mut self,option:u8,argument:A) ->  ::core::result::Result<(),E>where A: ::debate_parser::ArgAccess<'arg> ,E: ::debate::state::Error<'arg,A>{
        let fields =  &mut self.fields;
        let position =  &mut self.position;
        match option {
            b'p' => match(match fields.0 {
                ::core::option::Option::None => match::debate::parameter::Parameter::present(argument){
                    ::core::result::Result::Err(err) =>  ::core::result::Result::Err(err),
                    ::core::result::Result::Ok(value) => {
                        fields.0 =  ::core::option::Option::Some(value);
                        ::core::result::Result::Ok(())
                    }

                    }
                ::core::option::Option::Some(ref mut old) =>  ::debate::parameter::Parameter::add_present(old,argument),

                }){
                ::core::result::Result::Ok(()) =>  ::core::result::Result::Ok(()),
                ::core::result::Result::Err(err) =>  ::core::result::Result::Err(::debate::state::Error::parameter("path",err)),

                },
            b'v' => match(match fields.1 {
                ::core::option::Option::None => match::debate::parameter::Parameter::present(argument){
                    ::core::result::Result::Err(err) =>  ::core::result::Result::Err(err),
                    ::core::result::Result::Ok(value) => {
                        fields.1 =  ::core::option::Option::Some(value);
                        ::core::result::Result::Ok(())
                    }

                    }
                ::core::option::Option::Some(ref mut old) =>  ::debate::parameter::Parameter::add_present(old,argument),

                }){
                ::core::result::Result::Ok(()) =>  ::core::result::Result::Ok(()),
                ::core::result::Result::Err(err) =>  ::core::result::Result::Err(::debate::state::Error::parameter("verbose",err)),

                },
            b's' => match(match fields.2 {
                ::core::option::Option::None => match::debate::parameter::Parameter::present(argument){
                    ::core::result::Result::Err(err) =>  ::core::result::Result::Err(err),
                    ::core::result::Result::Ok(value) => {
                        fields.2 =  ::core::option::Option::Some(value);
                        ::core::result::Result::Ok(())
                    }

                    }
                ::core::option::Option::Some(ref mut old) =>  ::debate::parameter::Parameter::add_present(old,argument),

                }){
                ::core::result::Result::Ok(()) =>  ::core::result::Result::Ok(()),
                ::core::result::Result::Err(err) =>  ::core::result::Result::Err(::debate::state::Error::parameter("second_path",err)),

                },
            _ => {
                let argument = match(::debate::state::State::add_short(&mut fields.4,option,argument,)){
                    ::core::result::Result::Ok(()) => return::core::result::Result::Ok(()),
                    ::core::result::Result::Err(err) => match err {
                        ::debate::util::DetectUnrecognized::Unrecognized(argument) => argument,
                        ::debate::util::DetectUnrecognized::Error(error) => return::debate::state::Error::flattened("inner",error)
                        ;

                        }

                    };
                ::core::result::Result::Err(::debate::state::Error::unrecognized(argument))
            }

            }
    }

    }
impl <'arg> ::debate::from_args::BuildFromArgs<'arg>for Arguments {
    type State = __ArgumentsState<'arg> ;
    fn build<E>(state:Self::State) ->  ::core::result::Result<Self,E>where E: ::debate::from_args::Error<'arg>{
        let fields = state.fields;
        ::core::result::Result::Ok(Self {
            path:match fields.0 {
                ::core::option::Option::Some(value) => value,
                ::core::option::Option::None => match::debate::parameter::Parameter::absent(){
                    ::core::result::Result::Ok(value) => value,
                    ::core::result::Result::Err(::debate::parameter::RequiredError) => return::core::result::Result::Err(::debate::from_args::Error::required("path", ::core::option::Option::Some("foo"), ::core::option::Option::Some('p'))),

                    },

                },verbose:match fields.1 {
                ::core::option::Option::Some(value) => value,
                ::core::option::Option::None => match::debate::parameter::Parameter::absent(){
                    ::core::result::Result::Ok(value) => value,
                    ::core::result::Result::Err(::debate::parameter::RequiredError) => return::core::result::Result::Err(::debate::from_args::Error::required("verbose", ::core::option::Option::Some("verbose"), ::core::option::Option::Some('v'))),

                    },

                },second_path:match fields.2 {
                ::core::option::Option::Some(value) => value,
                ::core::option::Option::None => match::debate::parameter::Parameter::absent(){
                    ::core::result::Result::Ok(value) => value,
                    ::core::result::Result::Err(::debate::parameter::RequiredError) => return::core::result::Result::Err(::debate::from_args::Error::required("second_path", ::core::option::Option::None, ::core::option::Option::Some('s'))),

                    },

                },value:match fields.3 {
                ::core::option::Option::Some(value) => value,
                ::core::option::Option::None =>  ::core::default::Default::default(),

                },inner:match::debate::from_args::BuildFromArgs::build(fields.4){
                ::core::result::Result::Ok(value) => value,
                ::core::result::Result::Err(err) => return::core::result::Result::Err(err),

                },input:match fields.5 {
                ::core::option::Option::Some(value) => value,
                ::core::option::Option::None => match::debate::parameter::Parameter::absent(){
                    ::core::result::Result::Ok(value) => value,
                    ::core::result::Result::Err(::debate::parameter::RequiredError) => return::core::result::Result::Err(::debate::from_args::Error::required("input", ::core::option::Option::None, ::core::option::Option::None)),

                    },

                },other_inputs:match fields.6 {
                ::core::option::Option::Some(value) => value,
                ::core::option::Option::None => match::debate::parameter::Parameter::absent(){
                    ::core::result::Result::Ok(value) => value,
                    ::core::result::Result::Err(::debate::parameter::RequiredError) => return::core::result::Result::Err(::debate::from_args::Error::required("other_inputs", ::core::option::Option::None, ::core::option::Option::None)),

                    },

                },
        })
    }

    }

#[derive(FromArgs, Debug)]
#[debate(help, author)]
struct Arguments {
    #[debate(short, long = "foo")]
    path: PathBuf,

    #[debate(short, long)]
    verbose: bool,

    #[debate(short)]
    second_path: Option<PathBuf>,

    #[debate(long = "cool-value", default)]
    value: i32,

    #[debate(flatten)]
    inner: Alphabet,

    input: String,
    other_inputs: Vec<String>,
}

#[derive(FromArgs, Debug)]
struct Alphabet {
    #[debate(long, short, default)]
    alpha: u32,

    #[debate(long, short, default)]
    beta: u32,

    #[debate(long, short, default)]
    gamma: u32,

    #[debate(long)]
    direction: Option<Direction>,
}

#[derive(Debug, Clone, Copy, Value)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

enum FlagChoice {
    OutDir(PathBuf),
    WorkingDir,
    OutFile(PathBuf),
}

#[derive(FromArgs)]
#[debate(subcommand)]
enum Subcommand {
    Clean,
    Build { target: PathBuf },
    Add { item: String },
}

// enum SubcommandFieldState {
//     Clean,
//     Build { target: Option<PathBuf> },
//     Add { item: Option<String> },
// }

// #[derive(Default)]
// struct SubcommandState {
//     fields: Option<SubcommandFieldState>,
//     position: u16,
// }

// impl<'arg> State<'arg> for SubcommandState {
//     fn add_positional<E>(&mut self, argument: Arg<'arg>) -> Result<(), E>
//     where
//         E: state::Error<'arg, ()>,
//     {
//         match self.fields {
//             None => {
//                 self.fields = match argument.bytes() {
//                     b"clean" => SubcommandFieldState::Clean,
//                     b"build" => SubcommandFieldState::Build { target: None },
//                     b"add" => SubcommandFieldState::Add { item: None },
//                     _ => return Err(E::unrecognized(())),
//                 };

//                 Ok(())
//             }
//             Some(state) => match state {
//                 SubcommandFieldState::Clean => Err(E::unrecognized(())),
//                 SubcommandFieldState::Build { target } => Err(E::unrecognized(())),
//                 SubcommandFieldState::Add { ref mut item } => {
//                     if self.position <= 0 {
//                         match match item {
//                             None => match Parameter::arg(argument) {
//                                 Ok(value) => {
//                                     *item = Some(value);
//                                     Ok(())
//                                 },
//                                 Err(err),
//                             },
//                             Some(old) => Parameter::add_arg(old, argument),
//                         } {
//                             Ok(()) => return Ok(()),
//                             Err()
//                         }
//                     }
//                 }
//             },
//         }
//     }

//     fn add_long_option<E>(&mut self, option: Arg<'arg>, argument: Arg<'arg>) -> Result<(), E>
//     where
//         E: state::Error<'arg, ()>,
//     {
//         todo!()
//     }

//     fn add_long<A, E>(&mut self, option: Arg<'arg>, argument: A) -> Result<(), E>
//     where
//         A: debate_parser::ArgAccess<'arg>,
//         E: state::Error<'arg, A>,
//     {
//         todo!()
//     }

//     fn add_short<A, E>(&mut self, option: u8, argument: A) -> Result<(), E>
//     where
//         A: debate_parser::ArgAccess<'arg>,
//         E: state::Error<'arg, A>,
//     {
//         todo!()
//     }
// }

// impl<'arg> BuildFromArgs<'arg> for Subcommand {
//     type State = SubcommandState;

//     fn build<E>(state: Self::State) -> Result<Self, E>
//     where
//         E: from_args::Error<'arg>,
//     {
//         match state.fields {
//             None => Err(E::required("subcommand", None, None)),
//             Some(fields) => match fields {
//                 SubcommandFieldState::Clean => Ok(Self::Clean),
//                 SubcommandFieldState::Build { target } => Ok(Self::Build {
//                     target: match target {
//                         Some(target) => target,
//                         None => Parameter::absent()
//                             .map_err(|_| E::required("target", Some("target"), None))?,
//                     },
//                 }),
//                 SubcommandFieldState::Add { item } => Ok(Self::Add {
//                     item: match item {
//                         Some(item) => item,
//                         None => Parameter::absent()
//                             .map_err(|_| E::required("item", Some("item"), None))?,
//                     },
//                 }),
//             },
//         }
//     }
// }

#[derive(Debug, thiserror::Error)]
enum BuildError {
    #[error("required field {0:?} wasn't present")]
    Required(&'static str),

    #[error("{0}")]
    Custom(String),

    #[error("--{0}: {1}")]
    Long(String, StateError),

    #[error("-{0}: {1}")]
    Short(char, StateError),

    #[error("argument {0}: {1}")]
    Positional(String, StateError),
}

impl<'arg> from_args::Error<'arg> for BuildError {
    type StateError<A> = StateError;

    fn positional(argument: Arg<'arg>, error: Self::StateError<()>) -> Self {
        Self::Positional(
            String::from_utf8_lossy(argument.bytes()).into_owned(),
            error,
        )
    }

    fn long_with_argument(
        option: Arg<'arg>,
        _argument: Arg<'arg>,
        error: Self::StateError<()>,
    ) -> Self {
        Self::long::<()>(option, error)
    }

    fn long<A>(option: Arg<'arg>, error: Self::StateError<A>) -> Self {
        Self::Long(String::from_utf8_lossy(option.bytes()).into_owned(), error)
    }

    fn short<A>(option: u8, error: Self::StateError<A>) -> Self {
        Self::Short(option as char, error)
    }

    fn required(field: &'static str, long: Option<&'static str>, short: Option<char>) -> Self {
        Self::Required(field)
    }

    fn custom(msg: impl std::fmt::Display) -> Self {
        Self::Custom(msg.to_string())
    }
}

#[derive(Debug, thiserror::Error)]
enum StateError {
    #[error("error for field {0}: {1}")]
    Parameter(&'static str, ParameterError),

    #[error("unrecognized argument")]
    Unrecognized,
}

impl<'arg, A> state::Error<'arg, A> for StateError {
    type ParameterError = ParameterError;
    fn parameter(field: &'static str, error: Self::ParameterError) -> Self {
        Self::Parameter(field, error)
    }

    fn unrecognized(_: A) -> Self {
        Self::Unrecognized
    }

    fn flattened(field: &'static str, error: Self) -> Self {
        error
    }

    fn unknown_subcommand(expected: &'static [&'static str]) -> Self {
        Self::Parameter(
            "subcommand",
            ParameterError::Custom("invalid subcommand".to_owned()),
        )
    }

    fn rejected() -> Self {
        Self::Unrecognized
    }
}

#[derive(Debug, thiserror::Error)]
enum ParameterError {
    #[error("this parameter requires an argument")]
    NeedArg,

    #[error("this parameter doesn't take an argument (got {0})")]
    GotArg(String),

    #[error("this parameter appeared more than once on the command line")]
    GotExtra,

    #[error("argument contained invalid UTF8")]
    InvalidUtf8,

    #[error("error parsing argument {argument:?}: {error}")]
    ParseError { argument: String, error: String },

    #[error("error parsing raw bytes: {error}")]
    ByteParseError { error: String },

    #[error("{0}")]
    Custom(String),
}

impl<'arg> debate::parameter::Error<'arg> for ParameterError {
    fn needs_arg() -> Self {
        Self::NeedArg
    }
    fn got_arg(arg: Arg<'arg>) -> Self {
        Self::GotArg(String::from_utf8_lossy(arg.bytes()).into_owned())
    }

    fn got_additional_instance() -> Self {
        Self::GotExtra
    }

    fn invalid_utf8(_: Arg<'arg>) -> Self {
        Self::InvalidUtf8
    }

    fn parse_error(arg: &str, msg: impl std::fmt::Display) -> Self {
        Self::ParseError {
            argument: arg.to_string(),
            error: msg.to_string(),
        }
    }

    fn byte_parse_error(_: Arg<'arg>, error: impl std::fmt::Display) -> Self {
        Self::ByteParseError {
            error: error.to_string(),
        }
    }

    fn custom(msg: impl std::fmt::Display) -> Self {
        Self::Custom(msg.to_string())
    }
}

fn main() -> anyhow::Result<()> {
    let args: Vec<Vec<u8>> = std::env::args_os()
        .map(|arg| arg.into_encoded_bytes())
        .collect();

    let args: Result<Arguments, BuildError> = Arguments::from_args(ArgumentsParser::new(
        args.iter().skip(1).map(|arg| arg.as_slice()),
    ));

    let args = args.context("error parsing CLI argument")?;

    println!("{args:#?}");

    Ok(())
}
