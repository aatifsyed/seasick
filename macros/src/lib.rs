use core::fmt;

use attrs::*;
use proc_macro2::{Span, TokenStream};
use quote::{ToTokens, quote};
use syn::{
    Token,
    parse::{Parse, ParseStream},
    parse_quote,
    punctuated::Punctuated,
    spanned::Spanned,
    token,
};

#[proc_macro]
pub fn assert_abi(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    syn::parse_macro_input!(item with do_assert_abi).into()
}

fn do_assert_abi(input: ParseStream) -> syn::Result<TokenStream> {
    let mut krate: syn::Path = parse_quote!(seasick);
    if input.peek(Token![crate]) && (input.peek2(Token![=]) || input.peek2(token::Paren)) {
        input.parse::<Token![crate]>()?;
        with::peq(on::maybe_str(&mut krate))(input)?;
        input.parse::<Token![;]>()?;
    }
    let asserts = input.parse_terminated(AssertAbi::parse, Token![;])?;

    let mut checks = TokenStream::new();
    for AssertAbi {
        this:
            ref this @ Cast {
                path: _,
                as_token: _,
                bare_fn:
                    syn::TypeBareFn {
                        lifetimes: _,
                        ref unsafety,
                        ref abi,
                        fn_token: _,
                        paren_token: _,
                        ref inputs,
                        ref variadic,
                        ref output,
                    },
            },
        mut other,
    } in asserts
    {
        check_abi(abi.as_ref(), other.bare_fn.abi.as_ref())?;
        if let (None, Some(it)) = (unsafety, &other.bare_fn.unsafety) {
            return Err(syn::Error::new(
                it.span,
                "Safe fn may not be replaced by an unsafe fn",
            ));
        }
        if let (Some(var), _) | (_, Some(var)) = (variadic, &other.bare_fn.variadic) {
            return Err(syn::Error::new(
                var.span(),
                "Variadic functions are not supported",
            ));
        }
        if inputs.len() != other.bare_fn.inputs.len() {
            return Err(syn::Error::new(
                inputs.span(),
                format_args!(
                    "Mismatched function arity ({} vs {})",
                    inputs.len(),
                    other.bare_fn.inputs.len()
                ),
            ));
        }
        for (ix, param) in other.bare_fn.inputs.iter_mut().enumerate() {
            if matches!(param.ty, syn::Type::Infer(_)) {
                let ty = &inputs[ix].ty;
                param.ty = parse_quote!(#ty)
            }
        }
        for (ix, (this, other)) in inputs.iter().zip(&other.bare_fn.inputs).enumerate() {
            let this = &this.ty;
            let other = &other.ty;
            let bad_size = format!("Mismatched size in parameter {ix}");
            let bad_align = format!("Mismatched align in parameter {ix}");
            checks.extend(quote! {
                let this = Layout::new::<#this>();
                let other = Layout::new::<#other>();
                if this.size() != other.size() {
                    panic!(#bad_size)
                }
                if this.align() != other.align() {
                    panic!(#bad_align)
                }
            });
        }
        if let syn::ReturnType::Type(_, it) = &mut other.bare_fn.output {
            if let syn::Type::Infer(_) = &mut **it {
                other.bare_fn.output = parse_quote!(#output)
            }
        }
        let this_ret = return_type(output);
        let other_ret = return_type(&other.bare_fn.output);

        checks.extend(quote! {
            let this = Layout::new::<#this_ret>();
            let other = Layout::new::<#other_ret>();
            if this.size() != other.size() {
                panic!("Mismatched size in return type")
            }
            if this.align() != other.align() {
                panic!("Mismatched align in return type")
            }
            let _ = #this;
            let _ = #other;
        });
    }
    Ok(quote! {
        const _: () = {
            use #krate::{
                __private::{
                    core::{
                        alloc::Layout,
                        panic,
                    }
                }
            };

            #checks
        };
    })
}

fn return_type(it: &syn::ReturnType) -> syn::Type {
    match it {
        syn::ReturnType::Default => syn::Type::Tuple(syn::TypeTuple {
            paren_token: token::Paren::default(),
            elems: Punctuated::new(),
        }),
        syn::ReturnType::Type(_, it) => parse_quote!(#it),
    }
}

fn check_abi(left: Option<&syn::Abi>, right: Option<&syn::Abi>) -> syn::Result<()> {
    let left = abi_string(left);
    let right = abi_string(right);
    if left == right {
        return Ok(());
    }

    match (&*left, &*right) {
        ("C-unwind", "C") => Ok(()),
        _ => Err({
            let mut e = syn::Error::new(left.span(), "Mismatched ABI");
            e.combine(syn::Error::new(right.span(), "Mismatched ABI"));
            e
        }),
    }
}

fn abi_string(abi: Option<&syn::Abi>) -> String {
    match abi {
        Some(it) => match &it.name {
            Some(s) => s.value(),
            None => String::from("C"),
        },
        None => String::from("Rust"),
    }
}

struct AssertAbi {
    this: Cast,
    other: Cast,
}
impl Parse for AssertAbi {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            this: input.parse()?,
            other: {
                input.parse::<Token![==]>()?;
                input.parse()?
            },
        })
    }
}

struct Cast {
    path: syn::Path,
    as_token: Token![as],
    bare_fn: syn::TypeBareFn,
}

impl Parse for Cast {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            path: input.parse()?,
            as_token: input.parse()?,
            bare_fn: input.parse()?,
        })
    }
}

impl ToTokens for Cast {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            path,
            as_token,
            bare_fn,
        } = self;
        path.to_tokens(tokens);
        as_token.to_tokens(tokens);
        bare_fn.to_tokens(tokens);
    }
}

#[proc_macro_derive(TransmuteFrom, attributes(transmute))]
pub fn transmute_from(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    do_transmute(syn::parse_macro_input!(item as _), TransmuteKind::From)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

#[proc_macro_derive(TransmuteRefFrom, attributes(transmute))]
pub fn transmute_ref_from(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    do_transmute(syn::parse_macro_input!(item as _), TransmuteKind::RefFrom)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}
#[proc_macro_derive(TransmuteMutFrom, attributes(transmute))]
pub fn transmute_mut_from(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    do_transmute(syn::parse_macro_input!(item as _), TransmuteKind::MutFrom)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

enum TransmuteKind {
    From,
    MutFrom,
    RefFrom,
}

fn do_transmute(input: syn::DeriveInput, kind: TransmuteKind) -> syn::Result<TokenStream> {
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
            let local = #krate::__private::layout_of_field(|it: &#local|&it.#local_member);
            let remote = #krate::__private::layout_of_field(|it: &#remote| &it.#remote_member);

            if local.size() != remote.size() {
                #krate::__private::core::panic!(#bad_size)
            }
            if local.align() != remote.align() {
                #krate::__private::core::panic!(#bad_align)
            }
            if #krate::__private::core::mem::offset_of!(#local, #local_member)
                != #krate::__private::core::mem::offset_of!(#remote, #remote_member) {
                #krate::__private::core::panic!(#bad_offset)
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
                #krate::__private::layout_of_field::<#remote, #remote_ty>(|it| &it.#remote_member);
            });
        }

        remote_members.push(remote_member);
    }

    checks.extend(match kind {
        TransmuteKind::From => {
            let preamble = format!("`{local}` and (from) `{}`: mismatched", Fmt(&remote));
            let bad_size = format!("{preamble} size");
            let bad_align = format!("{preamble} alignment");

            quote! {
                fn exhaustive(#remote {
                    #(#remote_members: _,)*
                }: #remote) {}

                let local = #krate::__private::core::alloc::Layout::new::<#local>();
                let remote = #krate::__private::core::alloc::Layout::new::<#remote>();

                if local.size() != remote.size() {
                    #krate::__private::core::panic!(#bad_size)
                }
                if local.align() != remote.align() {
                    #krate::__private::core::panic!(#bad_align)
                }
            }
        }
        TransmuteKind::MutFrom | TransmuteKind::RefFrom => {
            let bad_align = format!(
                "`{local}` and (from) `{}`: mismatched alignment",
                Fmt(&remote)
            );
            quote! {
                let local = #krate::__private::core::alloc::Layout::new::<#local>();
                let remote = #krate::__private::core::alloc::Layout::new::<#remote>();

                if local.align() != remote.align() {
                    #krate::__private::core::panic!(#bad_align)
                }
            }
        }
    });

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let impls = match kind {
        TransmuteKind::From => quote! {
            unsafe impl #impl_generics #krate::TransmuteFrom<#remote> for #local #ty_generics #where_clause {
                unsafe fn transmute_from(remote: #remote) -> Self {
                    unsafe { #krate::__private::core::mem::transmute::<#remote, #local>(remote) }
                }
            }
            unsafe impl #impl_generics #krate::TransmuteFrom<#local #ty_generics> for #remote #where_clause {
                unsafe fn transmute_from(local: #local) -> Self {
                    unsafe { #krate::__private::core::mem::transmute::<#local, #remote>(local) }
                }
            }
        },
        TransmuteKind::MutFrom => quote! {
            unsafe impl #impl_generics #krate::TransmuteMutFrom<#remote> for #local #ty_generics #where_clause {
                unsafe fn transmute_mut(remote: &mut #remote) -> &mut Self {
                    let ptr = remote as *mut #remote as *mut Self;
                    unsafe { &mut *ptr }
                }
            }
        },
        TransmuteKind::RefFrom => quote! {
            unsafe impl #impl_generics #krate::TransmuteRefFrom<#remote> for #local #ty_generics #where_clause {
                unsafe fn transmute_ref(remote: &#remote) -> &Self {
                    let ptr = remote as *const #remote as *const Self;
                    unsafe { &*ptr }
                }
            }
        },
    };

    Ok(quote! {
        const _: () = {
            #checks
            #impls
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
