mod common;
mod from_args;
mod generics;
mod main_func;
mod usage;
mod value;

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;

macro_rules! proc_macro_definition {
    (
        derive($Trait:ident) attributes($($attrs:tt)*)
        pub fn $local_fn_name:ident => $implementation:path;
    ) => {
        #[proc_macro_derive($Trait, attributes($($attrs)*))]
        pub fn $local_fn_name(item: TokenStream) -> TokenStream {
            let item: TokenStream2 = item.into();
            let result: syn::Result<TokenStream2> = $implementation(item);
            match result {
                Ok(tokens) => tokens.into(),
                Err(err) => err.to_compile_error().into(),
            }
        }
    }
}

// TODO: separate `BuildFromArgs` and `Usage` derive, with `FromArgs` covering
// both

proc_macro_definition! {
    derive(FromArgs) attributes(debate)
    pub fn derive_args => from_args::derive_args_result;
}

proc_macro_definition! {
    derive(Value) attributes(debate)
    pub fn derive_value => value::derive_value_result;
}

proc_macro_definition! {
    derive(Usage) attributes(debate, doc)
    pub fn derive_usage => usage::derive_usage_result;
}

proc_macro_definition! {
    derive(ParameterUsage) attributes()
    pub fn derive_parameter_usage => usage::value::derive_parameter_usage_result;
}

/*
    fn main(args: Args) -> Foo {
        $BODY
    }

    becomes

    fn main() {
        let args = LoadedArguments::from_env();
        let args: Args = args.parse();

        $BODY
    }
*/

// TODO: reuse the macro from above, it's close enough.
/// Decorate an `fn main` to inject the command line arguments as a type.
#[proc_macro_attribute]
pub fn main(attrs: TokenStream, item: TokenStream) -> TokenStream {
    match main_func::decorate_fn_main(attrs.into(), item.into()) {
        Ok(tokens) => tokens,
        Err(err) => err.to_compile_error(),
    }
    .into()
}
