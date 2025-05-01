use core::fmt;

use attrs::*;
use proc_macro2::TokenStream;
use quote::quote;
use syn::{parse_quote, spanned::Spanned};

#[proc_macro_derive(AssertAbi, attributes(assert_abi))]
pub fn derive_assert_abi(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    expand(syn::parse_macro_input!(item as _))
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

fn expand(input: syn::DeriveInput) -> syn::Result<TokenStream> {
    let syn::DeriveInput {
        attrs,
        vis: _,
        ident,
        generics: _,
        data,
    } = input;

    let syn::DataStruct {
        struct_token: _,
        fields,
        semi_token: _,
    } = match data {
        syn::Data::Struct(it) => it,
        syn::Data::Enum(syn::DataEnum { enum_token, .. }) => {
            return Err(syn::Error::new(
                enum_token.span,
                "Only `struct` is supported",
            ));
        }
        syn::Data::Union(syn::DataUnion { union_token, .. }) => {
            return Err(syn::Error::new(
                union_token.span,
                "Only `struct` is supported",
            ));
        }
    };

    let mut abi = None::<syn::Path>;
    let mut self_ = None::<syn::Path>;
    let mut core = None::<syn::Path>;
    let mut exhaustive = false;

    Attrs::new()
        .once("abi", with::peq(set::maybe_str(&mut abi)))
        .once("self", with::peq(set::maybe_str(&mut self_)))
        .once("exhaustive", flag::or_peq(&mut exhaustive))
        .once("core", with::peq(set::maybe_str(&mut core)))
        .parse_attrs("assert_abi", &attrs)?;

    let Some(abi) = abi else {
        return Err(syn::Error::new(
            ident.span(),
            "ABI type must be specified as `#[assert_abi(abi = ...)]`",
        ));
    };
    let self_ = self_.unwrap_or(ident.into());
    let core = core.unwrap_or(parse_quote!(::core));
    let exhaustive = exhaustive;

    let mut check_fields = TokenStream::new();
    let mut abi_fields = vec![];

    for (
        ix,
        syn::Field {
            attrs,
            vis: _,
            mutability: _,
            ident,
            colon_token: _,
            ty: _,
        },
    ) in fields.into_iter().enumerate()
    {
        let self_member = match ident {
            Some(it) => it.into(),
            None => syn::Member::from(ix),
        };

        let mut abi_member = None::<syn::Member>;
        let mut abi_ty = None::<syn::Type>;
        let mut skip = false;

        Attrs::new()
            .once("field", with::peq(set::maybe_str(&mut abi_member)))
            .alias("member", "field")
            .once("type", with::peq(set::maybe_str(&mut abi_ty)))
            .once("skip", flag::or_peq(&mut skip))
            .parse_attrs("assert_abi", &attrs)?;

        if skip {
            if abi_member.is_some() || abi_ty.is_some() {
                return Err(syn::Error::new(
                    self_member.span(),
                    "`#[assert_abi(skip)]` may not have additional arguments",
                ));
            }
            continue;
        }

        let abi_member = abi_member.unwrap_or(parse_quote!(#self_member));

        let preamble = format!(
            "Struct {} and (abi) {} have different",
            Fmt(&self_member),
            Fmt(&abi_member)
        );

        let bad_size = format!("{preamble} sizes");
        let bad_align = format!("{preamble} alignments");
        let bad_offset = format!("{preamble} offsets");

        check_fields.extend(quote! {{
            let this = layout_of_field(|it: &#self_| &it.#self_member);
            let abi = layout_of_field(|it: &#abi| &it.#abi_member);

            if this.size() != abi.size() {
                panic!(#bad_size)
            }
            if this.align() != abi.align() {
                panic!(#bad_align)
            }
            if offset_of!(#self_, #self_member) != offset_of!(#abi, #abi_member) {
                panic!(#bad_offset)
            }
        }});

        abi_fields.push((abi_member, abi_ty));
    }

    let check_exhaustive = match exhaustive {
        true => {
            let destructure = abi_fields
                .iter()
                .map(|(member, _)| quote!(#member: _))
                .collect::<Vec<_>>();
            quote! {
                fn check_exhaustive(#abi {#(#destructure,)*}: #abi) {}
            }
        }
        false => TokenStream::new(),
    };

    let check_types = abi_fields
        .iter()
        .filter_map(|(member, ty)| Some((member, ty.as_ref()?)))
        .map(|(member, ty)| {
            quote! {
                layout_of_field::<#abi, #ty>(|it|&it.#member);
            }
        });

    Ok(quote! {
        const _: () = {
            use #core::{alloc::Layout, panic, mem::offset_of};

            const fn layout_of_field<T, U>(_: fn(&T) -> &U) -> Layout {
                Layout::new::<U>()
            }

            #check_fields
            #check_exhaustive
            #(#check_types)*

            {
                let abi = Layout::new::<#abi>();
                let this = Layout::new::<#self_>();

                if this.size() != abi.size() {
                    panic!("Struct and abi type have different sizes")
                }
                if this.align() != abi.align() {
                    panic!("Struct and abi type have different alignments")
                }
            }
        };
    })
}

struct Fmt<T>(T);

impl fmt::Display for Fmt<&syn::Member> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            syn::Member::Named(ident) => f.write_fmt(format_args!("field `{ident}`")),
            syn::Member::Unnamed(syn::Index { index, .. }) => {
                f.write_fmt(format_args!("member at index {index}"))
            }
        }
    }
}
