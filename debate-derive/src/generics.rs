use std::collections::HashSet;

use itertools::Itertools;
use proc_macro::TokenTree;
use proc_macro2::{Punct, Span, TokenStream as TokenStream2, TokenTree as TokenTree2};
use quote::{ToTokens, quote};
use syn::{
    AngleBracketedGenericArguments, AssocConst, AssocType, Expr, GenericArgument, GenericParam,
    Generics, Ident, Lifetime, LifetimeParam, Type, TypeGroup, TypeParamBound, TypeParen, TypePtr,
    TypeReference, TypeSlice, WherePredicate, parse_quote, spanned::Spanned,
};

// pub struct ComputedGenerics {
//     pub impl_block_generics: TokenStream2,
//     pub type_generics: TokenStream2,
//     pub type_generics_with_lt: TokenStream2,
//     pub where_clause: TokenStream2,
//     pub lifetime: Lifetime,
// }

// fn const_includes_generic(
//     expr: &Expr,
//     generic_types: &HashSet<&Ident>,
//     generic_consts: &HashSet<&Ident>,
// ) -> bool {
//     match expr {
//         Expr::Lit(_) => false,
//         _ => true,
//     }
// }

// fn angle_bracketed_includes_generic(
//     generics: &AngleBracketedGenericArguments,

//     generic_types: &HashSet<&Ident>,
//     generic_consts: &HashSet<&Ident>,
// ) -> bool {
//     generics.args.iter().any(|arg| match arg {
//         GenericArgument::Lifetime(lifetime) => false,
//         GenericArgument::Type(ty) | GenericArgument::AssocType(AssocType { ty, .. }) => {
//             type_includes_generic(ty, generic_types, generic_consts)
//         }
//         GenericArgument::Const(value) | GenericArgument::AssocConst(AssocConst { value, .. }) => {
//             const_includes_generic(value, generic_types, generic_consts)
//         }
//         GenericArgument::Constraint(constraint) => true,
//         _ => todo!(),
//     })
// }

// /// Determine if a type needs to have a generic bound attached to it. This
// /// function reserves the right to return false positives in cases where I
// /// really can't be bothered to do the complete analysis
// fn type_includes_generic(
//     ty: &Type,
//     generic_types: &HashSet<&Ident>,
//     generic_consts: &HashSet<&Ident>,
// ) -> bool {
//     match ty {
//         Type::Path(path) => {
//             if path.qself.is_some() {
//                 return true;
//             }

//             if let Ok(segment) = path.path.segments.iter().exactly_one() {
//                 if generic_types.contains(&segment.ident) {
//                     return true;
//                 }
//             }

//             // Need to test the generics
//             path.path
//                 .segments
//                 .iter()
//                 .any(|segment| match segment.arguments {
//                     syn::PathArguments::None => false,
//                     syn::PathArguments::AngleBracketed(args) => todo!(),
//                     syn::PathArguments::Parenthesized(args) => todo!(),
//                 })
//         }
//         Type::Array(array) => {
//             type_includes_generic(&array.elem, generic_types, generic_consts)
//                 || const_includes_generic(&array.len, generic_types, generic_consts)
//         }
//         Type::BareFn(func) => {
//             func.inputs
//                 .iter()
//                 .any(|param| type_includes_generic(&param.ty, generic_types, generic_consts))
//                 || match func.output {
//                     syn::ReturnType::Default => false,
//                     syn::ReturnType::Type(_, ref output) => {
//                         type_includes_generic(&output, generic_types, generic_consts)
//                     }
//                 }
//         }
//         Type::Group(TypeGroup { elem, .. })
//         | Type::Paren(TypeParen { elem, .. })
//         | Type::Ptr(TypePtr { elem, .. })
//         | Type::Reference(TypeReference { elem, .. })
//         | Type::Slice(TypeSlice { elem, .. }) => {
//             type_includes_generic(&elem, generic_types, generic_consts)
//         }
//         Type::ImplTrait(_) | Type::TraitObject(_) => true,
//         Type::Infer(_) => true,
//         Type::Macro(_) => true,
//         Type::Never(_) => false,
//         Type::Tuple(tuple) => tuple
//             .elems
//             .iter()
//             .any(|ty| type_includes_generic(ty, generic_types, generic_consts)),
//         Type::Verbatim(_) => true,
//         _ => true,
//     }
// }

// pub fn compute_generics<'a>(
//     generics: &Generics,
//     types: impl IntoIterator<Item = &'a Type>,
//     bound: impl FnOnce(&Lifetime) -> TokenStream2,
// ) -> syn::Result<ComputedGenerics> {
//     let mut augmented_generics = generics.clone();

//     // This is probably not the most efficient way to do this, but
//     // `split_for_impl` is just too complex and too valuable for me to
//     // implement a subset of it by hand.

//     if let Some(const_generic) = generics.const_params().next() {
//         return Err(syn::Error::new(
//             const_generic.span(),
//             "debate derives don't support const generics yet. File a ticket",
//         ));
//     }

//     let lifetime = match generics.lifetimes().at_most_one().map_err(|_| {
//         syn::Error::new(
//             generics.params.span(),
//             "debate derives can have at most one generic lifetime ",
//         )
//     })? {
//         Some(param) => param.lifetime.clone(),
//         None => {
//             let lifetime = Lifetime::new("'arg", Span::mixed_site());
//             augmented_generics.params.insert(
//                 0,
//                 syn::GenericParam::Lifetime(LifetimeParam::new(lifetime.clone())),
//             );
//             lifetime
//         }
//     };

//     // In theory, we could scan the types here and only add where clauses for
//     // the ones that mention the generics. The big block of commented code
//     // above is an initial attempt to do this. For now we don't bother anything
//     // other than to determine that there are ANY generics on this type.
//     if generics
//         .params
//         .iter()
//         .any(|param| matches!(param, GenericParam::Const(_) | GenericParam::Type(_)))
//     {
//         let where_clause = augmented_generics.make_where_clause();
//         let bound = bound(&lifetime);

//         // Type inference makes it difficult to use .extend here
//         for field_ty in types {
//             where_clause
//                 .predicates
//                 .push(parse_quote!( #field_ty : #bound ));
//         }
//     }

//     let (impl_generics, type_generics_with_lt, where_clause) = augmented_generics.split_for_impl();
//     let (_, type_generics, _) = generics.split_for_impl();

//     Ok(ComputedGenerics {
//         impl_block_generics: quote! {#impl_generics},
//         type_generics: quote! {#type_generics},
//         type_generics_with_lt: quote! { #type_generics_with_lt },
//         where_clause: quote! {#where_clause},
//         lifetime,
//     })

//     // Scan types for types that appear to include any of our generic type
// }

pub struct AngleBracedLifetime {
    lifetime: Lifetime,
}

impl ToTokens for AngleBracedLifetime {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        use proc_macro2::Spacing::*;

        tokens.extend([TokenTree2::Punct(Punct::new('<', Alone))]);
        self.lifetime.to_tokens(tokens);
        tokens.extend([TokenTree2::Punct(Punct::new('>', Alone))]);
    }
}

pub fn compute_generics(
    generics: &Generics,
) -> syn::Result<(Lifetime, Option<AngleBracedLifetime>)> {
    if let Some(param) = generics.const_params().next() {
        return Err(syn::Error::new(
            param.span(),
            "const generics aren't (yet) supported by debate",
        ));
    }

    if let Some(param) = generics.type_params().next() {
        return Err(syn::Error::new(
            param.span(),
            "generic types aren't (yet) supported by debate",
        ));
    }

    generics
        .lifetimes()
        .at_most_one()
        .map(|param| match param {
            None => (Lifetime::new("'arg", Span::mixed_site()), None),
            Some(param) => (
                param.lifetime.clone(),
                Some(AngleBracedLifetime {
                    lifetime: param.lifetime.clone(),
                }),
            ),
        })
        .map_err(|_| {
            syn::Error::new(
                generics.params.span(),
                "can't derive debate traits with more than one lifetime",
            )
        })
}
