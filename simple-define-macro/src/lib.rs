use proc_macro::{Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};

#[proc_macro]
pub fn simple_define_macro(input: TokenStream) -> TokenStream {
    // TODO -- What span should I be using?
    let let_tok = TokenTree::Ident(Ident::new("let", Span::call_site()));
    let name = TokenTree::Ident(Ident::new("defined_by_macro", Span::call_site()));
    let eq = TokenTree::Punct(Punct::new('=', Spacing::Alone));
    let val = TokenTree::Literal(Literal::u32_unsuffixed(22));
    let semicolon = TokenTree::Punct(Punct::new(';', Spacing::Alone));
    let stream = vec![let_tok, name, eq, val, semicolon];
    let group = TokenTree::Group(Group::new(Delimiter::None, TokenStream::from_iter(stream)));
    TokenStream::from(group)
}
