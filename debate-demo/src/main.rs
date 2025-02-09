use std::path::PathBuf;

use debate_derive::Args;

#[derive(Args)]
#[args(help, author)]
pub struct Arguments {
    #[arg(short, long)]
    path: PathBuf,

    #[arg(short, long)]
    verbose: bool,

    #[arg(short, long)]
    second_path: Option<PathBuf>,

    #[arg(long, default)]
    value: i32,

    input: String,
    other_inputs: Vec<String>,
}

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

fn main() {}

// impl Arguments {
//     pub fn from_args<E: ValueError>(args: &[&[u8]]) -> Result<Self, E> {
//         let mut args = primitives::Arguments::new(args.iter().copied());

//         #[derive(Default)]
//         struct State {
//             path: Option<PathBuf>,

//             // long, short
//             verbose: Option<bool>,

//             // long, short
//             second_path: Option<Option<PathBuf>>,

//             // long, short, default
//             value: Option<i32>,

//             input: Option<String>,
//             other_inputs: Option<Vec<String>>,

//             // Tracking for positional arguments
//             position: u32,
//         }

//         impl<'arg> primitives::Visitor<'arg> for &mut State {
//             // This should be a result
//             type Value = Result<(), E>;

//             fn visit_positional(self, argument: Arg<'arg>) -> Self::Value {
//                 todo!()
//             }

//             fn visit_long_option(self, option: Arg<'arg>, argument: Arg<'arg>) -> Self::Value {
//                 todo!()
//             }

//             fn visit_long(
//                 self,
//                 option: Arg<'arg>,
//                 arg: impl primitives::ArgAccess<'arg>,
//             ) -> Self::Value {
//                 todo!()
//             }

//             fn visit_short(self, option: u8, arg: impl primitives::ArgAccess<'arg>) -> Self::Value {
//                 match option {
//                     b'p' => {
//                         self.path = Some(match self.path.take() {
//                             None => Value::present(arg)?,
//                             Some(old) => Value::add_present(old, arg)?,
//                         });
//                         Ok(())
//                     }
//                 }
//             }
//         }

//         let mut state = State::default();

//         while let () = args.next_arg(&mut state)? {}

//         Ok(Self {
//             path: match state.path.take() {
//                 Some(value) => value,
//                 None => Value::absent()?,
//             },
//             verbose: match state.verbose.take() {
//                 Some(value) => value,
//                 None => Value::absent()?,
//             },
//             second_path: match state.second_path.take() {
//                 Some(value) => value,
//                 None => Value::absent()?,
//             },
//             value: match state.value.take() {
//                 Some(value) => value,
//                 None => Value::absent()?,
//             },
//             input: match state.input.take() {
//                 Some(value) => value,
//                 None => Value::absent()?,
//             },
//             other_inputs: match state.other_inputs.take() {
//                 Some(value) => value,
//                 None => Value::absent()?,
//             },
//         })
//     }
// }
