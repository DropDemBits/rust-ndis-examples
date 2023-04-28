//! vtable macro impl

use proc_macro::{Span, TokenStream};

use syn::{parse_macro_input, spanned::Spanned};

/// On traits: Adds a list of boolean constants correpsonding to whether a trait method is implemented or not.
/// Default bodies are required for each optional trait method.
/// For uniformity, trait methods still get an associated `HAS_{name}` constant, but it's always true
///
/// On trait impls: Fills out which trait methods were implemented or not.
#[proc_macro_attribute]
pub fn vtable(_attr: TokenStream, item: TokenStream) -> TokenStream {
    vtable_macro(item)
}

fn vtable_macro(item: TokenStream) -> TokenStream {
    let item = parse_macro_input!(item as syn::Item);

    let res: Result<TokenStream, syn::Error> = match item {
        syn::Item::Trait(item) => vtable_trait(item),
        syn::Item::Impl(item) => vtable_impl(item),
        _ => Err(syn::Error::new(
            item.span(),
            "vtable can only be applied to traits and trait impls",
        )),
    };

    match res {
        Ok(it) => it,
        Err(it) => it.into_compile_error().into(),
    }
}

fn vtable_trait(mut item: syn::ItemTrait) -> syn::Result<TokenStream> {
    let optional_methods_markers = item
        .items
        .iter()
        .flat_map(|trait_item| {
            let syn::TraitItem::Fn(trait_fn) = trait_item else { return None; };

            let has_default_impl = trait_fn.default.is_none();
            let original_name = &trait_fn.sig.ident;
            let name = format!("HAS_{}", heck::AsShoutySnekCase(original_name.to_string()));
            let has_ident = syn::Ident::new(&name, Span::call_site().into());

            let addendum = if has_default_impl {
                quote::quote! { #[doc="Always true since this method is required to be implemented"] }
            } else {
                quote::quote! {}
            };

            Some(syn::TraitItem::Const(syn::parse_quote! {
                #[doc=concat!("If `", stringify!(#original_name), "` was implemented.")]
                #addendum
                const #has_ident: bool = #has_default_impl;
            }))
        })
        .collect::<Vec<_>>();
    item.items.extend(optional_methods_markers);

    Ok(quote::quote! { #item }.into())
}

fn vtable_impl(mut item: syn::ItemImpl) -> syn::Result<TokenStream> {
    let implemented_methods = item
        .items
        .iter()
        .flat_map(|item| {
            let syn::ImplItem::Fn(impl_fn) = item else { return None };

            let original_name = &impl_fn.sig.ident;
            let name = format!("HAS_{}", heck::AsShoutySnekCase(original_name.to_string()));
            let has_ident = syn::Ident::new(&name, Span::call_site().into());

            Some(syn::ImplItem::Const(syn::parse_quote! {
                const #has_ident: bool = true;
            }))
        })
        .collect::<Vec<_>>();
    item.items.extend(implemented_methods);

    Ok(quote::quote! { #item }.into())
}
