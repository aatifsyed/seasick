use core::fmt;

use attrs::*;
use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{
    Token,
    parse::{Parse, ParseStream},
    parse_quote,
    spanned::Spanned,
};

#[proc_macro_derive(TransmuteFrom, attributes(transmute))]
pub fn transmute_from(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    expand_transmute_from(syn::parse_macro_input!(item as _))
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

fn expand_transmute_from(input: syn::DeriveInput) -> syn::Result<TokenStream> {
    let syn::DeriveInput {
        ref attrs,
        vis: _,
        ident: local,
        generics,
        data,
    } = input;
    let syn::DataStruct {
        struct_token: _,
        fields,
        semi_token: _,
    } = as_struct(data)?;

    let ContainerArgs {
        from: remote,
        krate,
        strict,
    } = ContainerArgs::parse(local.span(), attrs)?;

    let mut checks = TokenStream::new();
    let mut remote_members = vec![];

    for StructMember {
        ref attrs,
        member: local_member,
        ty: local_ty,
    } in struct_members(fields)
    {
        let (remote_member, remote_ty) = match FieldArgs::parse_attrs(attrs)? {
            FieldArgs::Skip => continue,
            FieldArgs::Field { member, ty } => (member.unwrap_or(parse_quote!(#local_member)), ty),
        };

        let preamble = format!(
            "`{local}.{}` and (from) `{}.{}`: mismatched",
            Fmt(&local_member),
            Fmt(&remote),
            Fmt(&remote_member)
        );
        let bad_size = format!("{preamble} size");
        let bad_align = format!("{preamble} alignment");
        let bad_offset = format!("{preamble} offset");

        checks.extend(quote! {
            let local = layout_of_field(|it: &#local|&it.#local_member);
            let remote = layout_of_field(|it: &#remote| &it.#remote_member);

            if local.size() != remote.size() {
                panic!(#bad_size)
            }
            if local.align() != remote.align() {
                panic!(#bad_align)
            }
            if offset_of!(#local, #local_member) != offset_of!(#remote, #remote_member) {
                panic!(#bad_offset)
            }
        });

        let remote_ty = match strict {
            true => Some(remote_ty.unwrap_or(local_ty)),
            false => remote_ty,
        };

        if let Some(remote_ty) = remote_ty {
            if strict && matches!(remote_ty, syn::Type::Infer(_)) {
                return Err(syn::Error::new(
                    remote_ty.span(),
                    "Inferred types may not be used with `strict`",
                ));
            }
            checks.extend(quote! {
                layout_of_field::<#remote, #remote_ty>(|it| &it.#remote_member);
            });
        }

        remote_members.push(remote_member);
    }

    checks.extend(quote! {
        fn exhaustive(#remote {
            #(#remote_members: _,)*
        }: #remote) {}
    });

    let preamble = format!("`{local}` and (from) `{}`: mismatched", Fmt(&remote));
    let bad_size = format!("{preamble} size");
    let bad_align = format!("{preamble} alignment");

    checks.extend(quote! {
        let local = Layout::new::<#local>();
        let remote = Layout::new::<#remote>();

        if local.size() != remote.size() {
            panic!(#bad_size)
        }
        if local.align() != remote.align() {
            panic!(#bad_align)
        }
    });

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    Ok(quote! {
        const _: () = {
            use #krate::{
                TransmuteFrom,
                __private::{
                    layout_of_field,
                    core::{
                        alloc::Layout,
                        panic,
                        mem::{offset_of, transmute},
                    }
                }
            };

            const _: () = { #checks };

            impl #impl_generics TransmuteFrom<#remote> for #local #ty_generics #where_clause {
                unsafe fn transmute_from(remote: #remote) -> Self {
                    unsafe { transmute::<#remote, #local>(remote) }
                }
            }
            impl #impl_generics TransmuteFrom<#local #ty_generics> for #remote #where_clause {
                unsafe fn transmute_from(local: #local) -> Self {
                    unsafe { transmute::<#local, #remote>(local) }
                }
            }
        };
    })
}

struct ContainerArgs {
    from: syn::Path,
    krate: syn::Path,
    strict: bool,
}

impl ContainerArgs {
    fn parse(span: Span, attrs: &[syn::Attribute]) -> syn::Result<Self> {
        let mut from = None;
        let mut krate = parse_quote!(seasick);
        let mut strict = false;

        Attrs::new()
            .once("from", with::peq(set::maybe_str(&mut from)))
            .once("crate", with::peq(on::maybe_str(&mut krate)))
            .once("strict", flag::or_peq(&mut strict))
            .parse_attrs("transmute", attrs)?;

        match from {
            Some(from) => Ok(Self {
                from,
                krate,
                strict,
            }),
            None => Err(syn::Error::new(span, "Requires `#[transmute(from = ...)]`")),
        }
    }
}

enum FieldArgs {
    Skip,
    Field {
        member: Option<syn::Member>,
        ty: Option<syn::Type>,
    },
}

impl FieldArgs {
    fn parse_attrs(attrs: &[syn::Attribute]) -> syn::Result<Self> {
        let attributes = &*attrs
            .iter()
            .filter(|it| it.meta.path().is_ident("transmute"))
            .collect::<Vec<_>>();
        match attributes {
            [] => Ok(Self::Field {
                member: None,
                ty: None,
            }),
            [one] => {
                let this = one.parse_args();
                match this {
                    Ok(t) => Ok(t),
                    Err(mut e) => Err({
                        let message = "Expected `#[transmute(skip)]`, `#[transmute($ident: $ty)` or `#[transmute($ty)`";
                        e.combine(syn::Error::new(one.span(), message));
                        e
                    }),
                }
            }
            [_, two, ..] => Err(syn::Error::new(
                two.span(),
                "Only one `#[transmute(..)]` attribute is permitted",
            )),
        }
    }
}

impl Parse for FieldArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        syn::custom_keyword!(skip);
        if (input.peek(syn::Ident) || input.peek(syn::LitInt))
            && input.peek2(Token![:])
            && !input.peek2(Token![::])
        {
            Ok(Self::Field {
                member: Some(input.parse()?),
                ty: Some({
                    input.parse::<Token![:]>()?;
                    input.parse()?
                }),
            })
        } else if input.peek(skip) && input.peek2(syn::parse::End) {
            input.parse::<skip>()?;
            Ok(Self::Skip)
        } else {
            Ok(Self::Field {
                member: None,
                ty: Some(input.parse()?),
            })
        }
    }
}

fn as_struct(data: syn::Data) -> syn::Result<syn::DataStruct> {
    match data {
        syn::Data::Struct(it) => Ok(it),
        syn::Data::Enum(syn::DataEnum { enum_token, .. }) => Err(syn::Error::new(
            enum_token.span,
            "Only `struct` is supported",
        )),
        syn::Data::Union(syn::DataUnion { union_token, .. }) => Err(syn::Error::new(
            union_token.span,
            "Only `struct` is supported",
        )),
    }
}

fn struct_members(fields: syn::Fields) -> impl Iterator<Item = StructMember> {
    fields
        .into_iter()
        .enumerate()
        .map(|(ix, field)| StructMember {
            attrs: field.attrs,
            member: match field.ident {
                Some(it) => it.into(),
                None => ix.into(),
            },
            ty: field.ty,
        })
}

struct StructMember {
    attrs: Vec<syn::Attribute>,
    member: syn::Member,
    ty: syn::Type,
}

struct Fmt<T>(T);

impl fmt::Display for Fmt<&syn::Member> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            syn::Member::Named(ident) => ident.fmt(f),
            syn::Member::Unnamed(syn::Index { index, .. }) => index.fmt(f),
        }
    }
}

impl fmt::Display for Fmt<&syn::Path> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let syn::Path {
            leading_colon,
            segments,
        } = self.0;
        if leading_colon.is_some() {
            f.write_str("::")?
        }
        let mut first = true;
        for syn::PathSegment {
            ident,
            arguments: _,
        } in segments
        {
            match first {
                true => first = false,
                false => f.write_str("::")?,
            }
            f.write_fmt(format_args!("{}", ident))?
        }
        Ok(())
    }
}
