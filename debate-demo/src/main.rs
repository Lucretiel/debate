#![allow(dead_code)]

use std::path::{Path, PathBuf};

use debate::{BuildFromArgs, ParameterUsage, Usage, Value};

/// This is a demo program for the debate CLI parser
///
/// Lorem ipsum dolor sit amet, consectetur adipiscing elit. Pellentesque
/// feugiat eleifend rhoncus. Suspendisse facilisis faucibus dictum. Cras
/// fermentum, justo in bibendum accumsan,
/// ipsum quam porta sapien, sit amet
/// dictum mauris lorem non leo. Aliquam ut erat id enim gravida rhoncus sed
/// dignissim libero. Nulla urna dui, condimentum quis nibh sed, tincidunt
/// pulvinar enim. Aenean in nibh metus. Phasellus diam risus, eleifend a odio
/// eget, imperdiet porttitor mauris. Sed tristique ligula auctor dolor
/// aliquam, sed placerat ipsum scelerisque. Mauris felis quam, interdum sit
/// amet dui quis, pulvinar faucibus ligula.
#[derive(Debug, BuildFromArgs, Usage)]
#[debate(help)]
struct DebateDemo<'arg> {
    /// The path, a flag
    #[debate(short, long = "foo", override)]
    path: &'arg Path,

    /// Whether or not we're running in verbose mode. Also a flag.
    #[debate(short, long)]
    verbose: bool,

    /// The second path
    #[debate(short, override)]
    second_path: Option<PathBuf>,

    /**
     * Intro material for the cool value.
     *
     * This flag is optional, and includes a --no-cool-value (via `invert`)
     * that allows it to be reverted.
     *
     * Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nunc dignissim
     * placerat dolor, eu semper leo venenatis ut. Morbi volutpat congue
     * dignissim. Vivamus aliquet vitae massa sed gravida. Phasellus vulputate
     * lacus vitae convallis ultricies. Nulla facilisi. Suspendisse leo turpis,
     * tincidunt ut nisl eu, bibendum efficitur magna.
     *
     * Suspendisse molestie
     * nulla vitae urna semper feugiat. Aliquam ultricies sagittis elementum.
     * Maecenas eu nisi a nisi rutrum elementum. Nulla finibus nisi et nunc
     * auctor ornare. Vivamus fringilla id quam in venenatis.
     */
    #[debate(long = "cool-value", default, invert)]
    value: i32,

    /// Nested options. These will appear as their own group in usage messages.
    #[debate(flatten)]
    alphabet_options: Alphabet,

    /// A subcommand; each command has its own flags
    #[debate(flatten)]
    subcommand: Subcommand<'arg>,

    /**
    An optional positional number.

    This is here to test error messages related to positionals and parse
    errors.
    */
    scale: Option<u32>,

    /// A positional argument
    extra: Option<String>,

    /// A list of possible items we're interested in.
    ///
    /// `--no-items` will completely erase this list.
    #[debate(short, long = "item", invert = "no-items")]
    items: Vec<String>,

    /// A target, either --release or --debug. Not optional.
    #[debate(flatten)]
    target: Target,

    /// A value with a default expression
    #[debate(long, default = 10)]
    probably_10: u32,
}

/**
Basic example of `derive(FromArgs)` on an enum. Each variant of this enum is
a mutually exclusive flag; in this case, --release or --debug
 */
#[derive(Debug, Usage, BuildFromArgs)]
#[debate(long)]
enum Target {
    /// Use debug mode
    Debug,

    /// Use release mode
    Release,
}

#[derive(BuildFromArgs, Usage, Debug)]
struct Alphabet {
    /// The alpha value
    #[debate(long, short, default)]
    alpha: u32,

    /// The best value
    #[debate(long, short, default)]
    beta: u32,

    /// The gamma value
    #[debate(long, short, default)]
    gamma: u32,

    /// The direction you
    /// Want to go
    #[debate(long)]
    direction: Option<Direction>,

    /// Even more deeply nested options
    #[debate(flatten)]
    special_alphabet_options: NestedAlphabet,
}

#[derive(BuildFromArgs, Usage, Debug)]
struct NestedAlphabet {
    /// A flag with only a short form, to test some stuff with short descriptions
    #[debate(short)]
    x: bool,

    /// A flag with a short form only
    #[debate(short)]
    y: bool,

    /// Brief header for z
    ///
    /// A flag with a short form but long description. Lorem ipsum dolor sit
    /// amet, consectetur adipiscing elit. Nunc dignissim placerat dolor, eu
    /// semper leo venenatis ut. Morbi volutpat congue dignissim. Vivamus
    /// aliquet vitae massa sed gravida. Phasellus vulputate lacus vitae
    /// convallis ultricies. Nulla facilisi. Suspendisse leo turpis, tincidunt
    /// ut nisl eu, bibendum efficitur magna.
    #[debate(short)]
    z: bool,
}

/**
An example of `derive(Value)` on an enum. This can appear as a command line
argument, where it will expect to be one of "up", "down", "left", or "right".
 */
#[derive(Debug, Clone, Copy, Value, ParameterUsage)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

/// Another example of `derive(Value)` on an enum
#[derive(Debug, Clone, Copy, Value, ParameterUsage)]
enum BuildMode {
    Debug,
    Release,
}

/**
A complex example of `derive(FromArgs)` on an enum. Each variant represents
either a single flag or a group of flags; each group is mutually exclusive with
the others. Notice, though, how one flag (--loud) appears in two different
variants; flags in these enums are allowed to appear in more than one variant,
so long as they're (more or less) identical in all cases.
 */
#[derive(BuildFromArgs, Debug, Usage)]
#[debate(long)]
enum FlagChoice {
    OutDir(PathBuf),

    #[debate(override, short)]
    WorkingDir,

    OutFile(PathBuf),

    Loud,

    Louder {
        /// The unit type makes this a switch, but a non-optional switch. This
        /// is normally pretty useless but it works here in enums to indicate
        /// that `--loud` MUST be present in the `Louder` variant.
        loud: (),
        shouter: String,
    },
}

/**
Example of `derive(FromArgs)` to create a group of subcommands. Note that
`#[debate(flatten)]` must be used to nest this into a parent arguments struct.

Note how each subcommand can have its own set of arguments, either directly
(shown in `Build` or via a newtype containing a `FromArgs` type (shown in
`Test` and `ExclusiveAction`).
*/
#[derive(BuildFromArgs, Debug, Usage)]
#[debate(subcommand)]
enum Subcommand<'arg> {
    /// If no subcommand is given, the fallback variant will be used. It's
    /// likely that I'm going to remove `fallback` in favor of just wrapping
    /// the subcommand in an `Option`.
    #[debate(fallback)]
    None,

    /// Run a basic `clean` of the project
    Clean,

    /// Build the project into a target directory
    ///
    /// These are more extensive docs for the build command.
    Build {
        /// The target directory to execute the build
        #[debate(long)]
        target: PathBuf,

        /// If given, the build mode. Should be either `debug` or `release`.
        #[debate(long)]
        mode: Option<BuildMode>,
    },

    /// Add an item to the project
    Add { item: &'arg str },

    /// Run the project's tests
    Test(TestArgs),

    /// A subcommand with a set of mututally exclusive arguments
    ExclusiveAction(FlagChoice),
}

#[derive(BuildFromArgs, Usage, Debug)]
struct TestArgs {
    /// If given, only run this test.
    ///
    /// Run all of them otherwise.
    #[debate(short, long)]
    test: Option<String>,

    /// If set, test failures will only produce warnings.
    #[debate(short = 'w', long)]
    warn_only: bool,
}

/// ``
#[debate::main]
fn main(args: DebateDemo<'_>) -> anyhow::Result<()> {
    println!("{args:#?}");

    Ok(())
}
