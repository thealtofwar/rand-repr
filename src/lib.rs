extern crate proc_macro;
use std::collections::HashSet;

use proc_macro::{Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};
use rand::{TryRngCore, rngs::OsRng};

fn compile_error(msg: &str) -> TokenStream {
    let mut out = TokenStream::new();

    // compile_error
    out.extend([TokenTree::Ident(Ident::new(
        "compile_error",
        Span::call_site(),
    ))]);

    // !
    out.extend([TokenTree::Punct(Punct::new('!', Spacing::Alone))]);

    let mut error_str = TokenStream::new();
    error_str.extend([Literal::string(msg)]);

    let group = Group::new(Delimiter::Parenthesis, error_str);
    out.extend([TokenTree::Group(group)]);

    out
}

fn is_enum(token_trees: &Vec<TokenTree>) -> bool {
    match &token_trees[0] {
        TokenTree::Ident(ident) => ident.to_string() == "enum",
        _ => false,
    }
}

#[allow(non_camel_case_types)] // keep enum variants consistent with types
#[derive(Clone, Copy)]
enum IntegralType {
    u32,
    i32,
    u64,
    i64,
}

impl IntegralType {
    fn gen_random(&self) -> Integral {
        match self {
            IntegralType::u32 => Integral::u32(OsRng.try_next_u32().unwrap()),
            IntegralType::i32 => Integral::i32(i32::from_ne_bytes(
                OsRng.try_next_u32().unwrap().to_ne_bytes(),
            )),
            IntegralType::u64 => Integral::u64(OsRng.try_next_u64().unwrap()),
            IntegralType::i64 => Integral::i64(i64::from_ne_bytes(
                OsRng.try_next_u64().unwrap().to_ne_bytes(),
            )),
        }
    }

    fn gen_repr_annotation(&self) -> TokenStream {
        let mut tree = TokenStream::new();

        // Create the repr(...) content
        let mut repr_content = TokenStream::new();
        repr_content.extend([TokenTree::Ident(Ident::new("repr", Span::call_site()))]);

        // Create the type content inside parentheses
        let mut type_content = TokenStream::new();
        let type_name = match self {
            IntegralType::u32 => "u32",
            IntegralType::i32 => "i32",
            IntegralType::u64 => "u64",
            IntegralType::i64 => "i64",
        };
        type_content.extend([TokenTree::Ident(Ident::new(type_name, Span::call_site()))]);

        // repr(type)
        repr_content.extend([TokenTree::Group(Group::new(
            Delimiter::Parenthesis,
            type_content,
        ))]);

        // Build #[repr(...)]
        tree.extend([
            TokenTree::Punct(Punct::new('#', Spacing::Alone)),
            TokenTree::Group(Group::new(Delimiter::Bracket, repr_content)),
        ]);

        tree
    }
}

#[allow(non_camel_case_types)] // keep enum variants consistent with types
#[derive(PartialEq, Eq, Hash, Clone, Copy)]
enum Integral {
    u32(u32),
    i32(i32),
    u64(u64),
    i64(i64),
}

impl Integral {
    fn to_literal(&self) -> Literal {
        match self {
            Integral::u32(i) => Literal::u32_suffixed(*i),
            Integral::i32(i) => Literal::i32_suffixed(*i),
            Integral::u64(i) => Literal::u64_suffixed(*i),
            Integral::i64(i) => Literal::i64_suffixed(*i),
        }
    }
}

fn parse_attrs(attrs: TokenStream) -> Result<IntegralType, &'static str> {
    if attrs.is_empty() {
        return Err(
            "this macro must be provided with a representation of the form `u32`, `i32`, `u64`, `i64`",
        );
    }
    let token_trees: Vec<TokenTree> = attrs.into_iter().collect();
    if token_trees.len() != 1 {
        return Err("this macro must be provided only one of `u32`, `i32`, `u64`, `i64`");
    }

    match &token_trees[0] {
        TokenTree::Ident(ident) => match ident.to_string().as_ref() {
            "u32" => Ok(IntegralType::u32),
            "i32" => Ok(IntegralType::i32),
            "u64" => Ok(IntegralType::u64),
            "i64" => Ok(IntegralType::i64),
            _ => Err("this macro must be provided one of `u32`, `i32`, `u64`, `i64`"),
        },
        _ => Err("this macro must be provided only `u32`, `i32`, `u64`, `i64`"),
    }
}

fn generate_unique_repr(
    generated_reprs: &mut HashSet<Integral>,
    integral_type: IntegralType,
) -> Integral {
    // TODO: Replace this with something like a block cipher to generate a random permutation deterministically instead of relying on retries
    loop {
        let integral = integral_type.gen_random();
        if generated_reprs.insert(integral) {
            return integral;
        }
    }
}

fn transform_token_tree(
    token_stream: TokenStream,
    generated_reprs: &mut HashSet<Integral>,
    integral_type: IntegralType,
) -> TokenStream {
    let mut result_token_stream = TokenStream::new();

    let mut last_token_tree: Option<TokenTree> = None;

    for child_token_tree in token_stream {
        if let TokenTree::Group(ref group) = child_token_tree {
            // recurse
            result_token_stream.extend([transform_token_tree(
                group.stream(),
                generated_reprs,
                integral_type,
            )]);
            continue;
        }

        if let TokenTree::Punct(ref punct) = child_token_tree
            && punct.as_char() == ','
            && last_token_tree.is_some_and(|last_tree| {
                matches!(last_tree, TokenTree::Ident(_) | TokenTree::Group(_))
            })
        {
            // insert an = num here
            result_token_stream.extend([
                TokenTree::Punct(Punct::new('=', Spacing::Alone)),
                TokenTree::Literal(
                    generate_unique_repr(generated_reprs, integral_type).to_literal(),
                ),
            ]);
        }
        result_token_stream.extend([child_token_tree.clone()]);
        last_token_tree = Some(child_token_tree.clone());
    }
    result_token_stream
}

#[proc_macro_attribute]
pub fn randomize_repr(attrs: TokenStream, item: TokenStream) -> TokenStream {
    let original_token_trees: Vec<TokenTree> = item.into_iter().collect();

    if !is_enum(&original_token_trees) {
        return compile_error("this macro must be called on an enum");
    }

    let attr_result = parse_attrs(attrs);

    if let Err(err) = attr_result {
        return compile_error(err);
    }

    let integral_type = attr_result.unwrap();

    let mut result_token_stream = TokenStream::new();

    result_token_stream.extend(integral_type.gen_repr_annotation());

    let mut generated_reprs: HashSet<Integral> = HashSet::new();

    result_token_stream
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn always_correct_types() {
        assert!(matches!(IntegralType::u32.gen_random(), Integral::u32(_)));
        assert!(matches!(IntegralType::u64.gen_random(), Integral::u64(_)));
        assert!(matches!(IntegralType::i32.gen_random(), Integral::i32(_)));
        assert!(matches!(IntegralType::i64.gen_random(), Integral::i64(_)));
    }
}
