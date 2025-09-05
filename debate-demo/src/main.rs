use std::path::PathBuf;

use debate_derive::{FromArgs, ParameterUsage, Usage, Value};

/// This is a demo program for the debate CLI parser
///
/// Lorem ipsum dolor sit amet, consectetur adipiscing elit. Pellentesque
/// feugiat eleifend rhoncus. Suspendisse facilisis faucibus dictum. Cras
/// fermentum, justo in bibendum accumsan, ipsum quam porta sapien, sit amet
/// dictum mauris lorem non leo. Aliquam ut erat id enim gravida rhoncus sed
/// dignissim libero. Nulla urna dui, condimentum quis nibh sed, tincidunt
/// pulvinar enim. Aenean in nibh metus. Phasellus diam risus, eleifend a odio
/// eget, imperdiet porttitor mauris. Sed tristique ligula auctor dolor
/// aliquam, sed placerat ipsum scelerisque. Mauris felis quam, interdum sit
/// amet dui quis, pulvinar faucibus ligula.
#[derive(Debug, FromArgs, Usage)]
#[debate(help)]
struct Arguments {
    /// The path
    #[debate(short, long = "foo")]
    path: PathBuf,

    /// Whether or not we're running in verbose mode
    #[debate(short, long)]
    verbose: bool,

    /// A flag with only a short form, to test some stuff with short descriptions
    #[debate(short)]
    x: bool,

    /// A flag with a short form only
    #[debate(short)]
    y: bool,

    /// A flag with a short form but long description. Lorem ipsum dolor sit
    /// amet, consectetur adipiscing elit. Nunc dignissim placerat dolor, eu
    /// semper leo venenatis ut. Morbi volutpat congue dignissim. Vivamus
    /// aliquet vitae massa sed gravida. Phasellus vulputate lacus vitae
    /// convallis ultricies. Nulla facilisi. Suspendisse leo turpis, tincidunt
    /// ut nisl eu, bibendum efficitur magna.
    #[debate(short)]
    z: bool,

    /// The second path
    #[debate(short)]
    second_path: Option<PathBuf>,

    /**
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
    #[debate(long = "cool-value", default)]
    value: i32,

    #[debate(flatten)]
    alphabet_options: Alphabet,

    #[debate(flatten)]
    subcommand: Subcommand,

    /// A positional argument
    extra: Option<String>,
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
}

#[derive(Debug, Clone, Copy, Value, ParameterUsage)]
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

#[derive(FromArgs, Usage, Debug)]
#[debate(subcommand)]
enum Subcommand {
    #[debate(fallback)]
    None,

    /// Run a basic `clean` of the project
    Clean,

    /// Build the project into a target directory
    Build {
        #[debate(long)]
        target: PathBuf,
    },

    /// Add an item to the project
    Add { item: String },
}

fn main() -> anyhow::Result<()> {
    let args = debate::arguments::LoadedArguments::from_env();
    let args: Arguments = args.parse();

    println!("{args:#?}");

    Ok(())
}
