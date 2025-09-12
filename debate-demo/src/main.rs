#![allow(dead_code)]

use std::path::{Path, PathBuf};

use debate_derive::{self, FromArgs, ParameterUsage, Usage, Value};

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
#[derive(Debug, FromArgs, Usage)]
#[debate(help)]
struct DebateDemo<'arg> {
    /// The path
    #[debate(short, long = "foo", override)]
    path: &'arg Path,

    /// Whether or not we're running in verbose mode
    #[debate(short, long)]
    verbose: bool,

    /// The second path
    #[debate(short, override)]
    second_path: Option<PathBuf>,

    /**
     * Intro material for the cool value
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

    #[debate(flatten)]
    alphabet_options: Alphabet,

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

    /// A list of possible items we're interested in
    #[debate(short, long = "item", invert = "no-items")]
    items: Vec<String>,
}

#[derive(FromArgs, Usage, Debug)]
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

#[derive(FromArgs, Usage, Debug)]
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

#[derive(Debug, Clone, Copy, Value, ParameterUsage)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

#[derive(Debug, Clone, Copy, Value, ParameterUsage)]
enum BuildMode {
    Debug,
    Release,
}

#[derive(FromArgs, Debug)]
#[debate(long)]
enum FlagChoice {
    OutDir(PathBuf),

    #[debate(override, short)]
    WorkingDir,

    OutFile(PathBuf),

    Loud {
        loud: (),
    },

    Louder {
        loud: (),
        shouter: String,
    },
}

// #[derive(FromArgs, Debug)]
// #[debate(long)]
// enum FlagChoice {
//     Loud { loud: (), shouter: String },
//     Quiet { quiet: (), requester: String },
// }

#[derive(FromArgs, Usage, Debug)]
#[debate(subcommand)]
enum Subcommand<'arg> {
    #[debate(fallback)]
    None,

    /// Run a basic `clean` of the project
    Clean,

    /// Build the project into a target directory
    ///
    /// These are more extensive docs for the build command
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
}

#[derive(FromArgs, Usage, Debug)]
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

#[debate_derive::main]
fn main(args: DebateDemo<'_>) -> anyhow::Result<()> {
    println!("{args:#?}");

    Ok(())
}
