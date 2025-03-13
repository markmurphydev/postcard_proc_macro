//! Serde trait derives for types in `rust-analyzer/crates/tt`

use std::num::NonZeroU32;

use intern::Symbol;
use salsa::{Id, plumbing::FromId};
use serde::{Deserialize, Serialize, Serializer, ser::SerializeSeq};
use span::{EditionedFileId, ErasedFileAstId, Span, SpanAnchor, SpanData, SyntaxContext};
use tt::{
    Delimiter, DelimiterKind, Ident, IdentIsRaw, Leaf, LitKind, Literal, Punct, Spacing, Subtree,
    SubtreeView, TextRange, TextSize, TokenTree, TopSubtree,
};

// Serde will infer the Serialize and Deserialize trait bounds for `S` if it's referenced directly (like in [DelimiterDef]),
//  but does _not_ infer the bounds for `S` otherwise when chaining remote definitions.
//  We add the bounds manually. See: https://serde.rs/attr-bound.html
// `'de` appears to be in-scope in the macro expansion, so it can be referred to in the `Deserialize` bound.
// TODO -- When Serde automatically inputs the bounds, it uses `_serde::Deserialize<'de>`.
//      Am I meant to use that one?

#[derive(Serialize, Deserialize)]
#[serde(bound(serialize = "S: Serialize", deserialize = "S: Deserialize<'de>"))]
pub struct TopSubtreeSerdeHelper<S>(#[serde(with = "TopSubtreeDef")] pub TopSubtree<S>);

impl TopSubtreeSerdeHelper<SpanDataDef<SyntaxContextDef>> {
    pub fn new(top_subtree: TopSubtree<Span>) -> Self {
        let mapped = top_subtree
            .0
            .into_iter()
            .map(Self::funcmap_wrap_span)
            .collect::<Vec<_>>()
            .into_boxed_slice();
        Self(TopSubtree(mapped))
    }

    /// Map over `token_tree`, wrapping each instance of `Span` in its ser/de wrapper
    fn funcmap_wrap_span(token_tree: TokenTree<Span>) -> TokenTree<SpanDataDef<SyntaxContextDef>> {
        match token_tree {
            TokenTree::Leaf(leaf) => TokenTree::Leaf(match leaf {
                Leaf::Literal(Literal {
                    symbol,
                    span,
                    kind,
                    suffix,
                }) => Leaf::Literal(Literal {
                    symbol,
                    span: Self::wrap_span(span),
                    kind,
                    suffix,
                }),
                Leaf::Punct(Punct {
                    char,
                    spacing,
                    span,
                }) => Leaf::Punct(Punct {
                    char,
                    spacing,
                    span: Self::wrap_span(span),
                }),
                Leaf::Ident(Ident { sym, span, is_raw }) => Leaf::Ident(Ident {
                    sym,
                    span: Self::wrap_span(span),
                    is_raw,
                }),
            }),
            TokenTree::Subtree(Subtree {
                delimiter: Delimiter { open, close, kind },
                len,
            }) => TokenTree::Subtree(Subtree {
                delimiter: Delimiter {
                    open: Self::wrap_span(open),
                    close: Self::wrap_span(close),
                    kind: kind,
                },
                len: len,
            }),
        }
    }

    /// Wrap the given `Span` in its ser/de wrapper
    fn wrap_span(Span { range, anchor, ctx }: Span) -> SpanDataDef<SyntaxContextDef> {
        let ctx_def = SyntaxContextDef(
            salsa::Id::from_u32(ctx.into_u32()),
            std::marker::PhantomData,
        );
        SpanDataDef {
            range,
            anchor,
            ctx: ctx_def,
        }
    }
}

// TODO -- make a helper for `Span`.
// REEEEEE IDK

#[derive(Serialize, Deserialize)]
#[serde(
    remote = "TopSubtree",
    bound(serialize = "S: Serialize", deserialize = "S: Deserialize<'de>")
)]
pub struct TopSubtreeDef<S>(
    #[serde(with = "serialize_box_slice_token_tree")] pub Box<[TokenTree<S>]>,
);

mod serialize_box_slice_token_tree {
    use super::TokenTreeDef;
    use serde::{Deserialize, Deserializer, Serialize, Serializer, ser::SerializeSeq};
    use tt::TokenTree;

    pub(super) fn serialize<Span: Serialize, Ser: Serializer>(
        value: &Box<[TokenTree<Span>]>,
        serializer: Ser,
    ) -> Result<Ser::Ok, Ser::Error> {
        #[derive(Serialize)]
        #[serde(bound(serialize = "Span: Serialize"))]
        struct Helper<'a, Span>(#[serde(with = "TokenTreeDef")] &'a TokenTree<Span>);

        let mut seq = serializer.serialize_seq(Some(value.len()))?;
        for token_tree in value.iter() {
            let h = Helper(token_tree);
            seq.serialize_element(&h)?;
        }
        seq.end()
    }

    pub(super) fn deserialize<'de, Span: Deserialize<'de>, D: Deserializer<'de>>(
        deserializer: D,
    ) -> Result<Box<[TokenTree<Span>]>, D::Error> {
        #[derive(Deserialize)]
        #[serde(bound(deserialize = "Span: Deserialize<'de>"))]
        struct Helper<Span>(#[serde(with = "TokenTreeDef")] TokenTree<Span>);

        let helper = Vec::deserialize(deserializer)?;
        Ok(helper
            .into_iter()
            .map(|Helper(token_tree)| token_tree)
            .collect::<Vec<_>>()
            .into_boxed_slice())
    }
}

#[derive(Serialize)]
#[serde(remote = "SubtreeView", bound(serialize = "S: Serialize"))]
pub struct SubtreeViewDef<'a, S: Copy>(
    #[serde(
        getter = "subtree_view_get_inner",
        serialize_with = "serialize_slice_token_tree"
    )]
    &'a [TokenTree<S>],
);

fn subtree_view_get_inner<'a, S: Copy>(view: &'_ SubtreeView<'a, S>) -> &'a [TokenTree<S>] {
    view.as_token_trees().flat_tokens()
}

fn serialize_slice_token_tree<'a, Span: Serialize, Ser: Serializer>(
    value: &'_ &'a [TokenTree<Span>],
    serializer: Ser,
) -> Result<Ser::Ok, Ser::Error> {
    #[derive(Serialize)]
    struct Helper<'a, Span: Serialize>(#[serde(with = "TokenTreeDef")] &'a TokenTree<Span>);

    let mut seq = serializer.serialize_seq(Some(value.len()))?;
    for tt in value.iter() {
        let h = Helper(tt);
        seq.serialize_element(&h)?;
    }
    seq.end()
}

#[derive(Serialize, Deserialize)]
#[serde(
    remote = "TokenTree",
    bound(serialize = "S: Serialize", deserialize = "S: Deserialize<'de>")
)]
pub enum TokenTreeDef<S = u32> {
    #[serde(with = "LeafDef")]
    Leaf(Leaf<S>),
    #[serde(with = "SubtreeDef")]
    Subtree(Subtree<S>),
}

#[derive(Serialize, Deserialize)]
#[serde(
    remote = "Leaf",
    bound(serialize = "S: Serialize", deserialize = "S: Deserialize<'de>")
)]
pub enum LeafDef<S> {
    #[serde(with = "LiteralDef")]
    Literal(Literal<S>),
    #[serde(with = "PunctDef")]
    Punct(Punct<S>),
    #[serde(with = "IdentDef")]
    Ident(Ident<S>),
}

#[derive(Serialize, Deserialize)]
#[serde(
    remote = "Subtree",
    bound(serialize = "S: Serialize", deserialize = "S: Deserialize<'de>")
)]
pub struct SubtreeDef<S> {
    #[serde(with = "DelimiterDef")]
    pub delimiter: Delimiter<S>,
    /// Number of following token trees that belong to this subtree, excluding this subtree.
    pub len: u32,
}

#[derive(Serialize, Deserialize)]
#[serde(remote = "Literal")]
pub struct LiteralDef<S> {
    #[serde(with = "symbol_def")]
    pub symbol: Symbol,
    pub span: S,
    #[serde(with = "LitKindDef")]
    pub kind: LitKind,
    #[serde(with = "option_symbol_def")]
    pub suffix: Option<Symbol>,
}

#[derive(Serialize, Deserialize)]
#[serde(remote = "Punct")]
pub struct PunctDef<S> {
    pub char: char,
    #[serde(with = "SpacingDef")]
    pub spacing: Spacing,
    pub span: S,
}

#[derive(Serialize, Deserialize)]
#[serde(remote = "Ident")]
pub struct IdentDef<S> {
    #[serde(with = "symbol_def")]
    pub sym: Symbol,
    pub span: S,
    #[serde(with = "IdentIsRawDef")]
    pub is_raw: IdentIsRaw,
}

#[derive(Serialize, Deserialize)]
#[serde(remote = "Delimiter")]
pub struct DelimiterDef<S> {
    pub open: S,
    pub close: S,
    #[serde(with = "DelimiterKindDef")]
    pub kind: DelimiterKind,
}

/// TODO -- Does proc_macro_srv have its own symbol intern table? I guess it must, if it uses the `Symbol` type.
mod symbol_def {
    use intern::Symbol;
    use serde::{Deserialize, Deserializer, Serialize, Serializer};

    pub(super) fn serialize<S: Serializer>(
        value: &Symbol,
        serializer: S,
    ) -> Result<S::Ok, S::Error> {
        value.as_str().serialize(serializer)
    }

    pub(super) fn deserialize<'de, D: Deserializer<'de>>(
        deserializer: D,
    ) -> Result<Symbol, D::Error> {
        let value = String::deserialize(deserializer)?;
        Ok(Symbol::intern(value.as_str()))
    }
}

/// TODO -- Can this be worked around with a #[serde(skip)] annotation?
mod option_symbol_def {
    use intern::Symbol;
    use serde::{Deserialize, Deserializer, Serialize, Serializer};

    pub(super) fn serialize<S: Serializer>(
        value: &Option<Symbol>,
        serializer: S,
    ) -> Result<S::Ok, S::Error> {
        value.as_ref().map(Symbol::as_str).serialize(serializer)
    }

    pub(super) fn deserialize<'de, D: Deserializer<'de>>(
        deserializer: D,
    ) -> Result<Option<Symbol>, D::Error> {
        let value = Option::deserialize(deserializer)?;
        Ok(value.map(|value: String| Symbol::intern(value.as_str())))
    }
}

#[derive(Serialize, Deserialize)]
#[serde(remote = "LitKind")]
pub enum LitKindDef {
    Byte,
    Char,
    Integer, // e.g. `1`, `1u8`, `1f32`
    Float,   // e.g. `1.`, `1.0`, `1e3f32`
    Str,
    StrRaw(u8), // raw string delimited by `n` hash symbols
    ByteStr,
    ByteStrRaw(u8), // raw byte string delimited by `n` hash symbols
    CStr,
    CStrRaw(u8),
    Err(()),
}

#[derive(Serialize, Deserialize)]
#[serde(remote = "Spacing")]
pub enum SpacingDef {
    Alone,
    Joint,
    JointHidden,
}

#[derive(Serialize, Deserialize)]
#[serde(remote = "IdentIsRaw")]
pub enum IdentIsRawDef {
    No,
    Yes,
}

#[derive(Serialize, Deserialize)]
#[serde(remote = "DelimiterKind")]
pub enum DelimiterKindDef {
    Parenthesis,
    Brace,
    Bracket,
    Invisible,
}

#[derive(Serialize, Deserialize, Clone, Copy)]
pub struct SpanDataDef<Ctx> {
    #[serde(with = "TextRangeDef")]
    pub range: TextRange,
    #[serde(with = "SpanAnchorDef")]
    pub anchor: SpanAnchor,
    pub ctx: Ctx,
}

#[derive(Serialize, Deserialize)]
#[serde(remote = "TextRange")]
pub struct TextRangeDef {
    // Invariant: start <= end
    #[serde(getter = "text_range_get_start", with = "TextSizeDef")]
    start: TextSize,
    #[serde(getter = "text_range_get_end", with = "TextSizeDef")]
    end: TextSize,
}

fn text_range_get_start(value: &TextRange) -> TextSize {
    value.start()
}

fn text_range_get_end(value: &TextRange) -> TextSize {
    value.end()
}

impl From<TextRangeDef> for TextRange {
    fn from(value: TextRangeDef) -> Self {
        TextRange::new(value.start, value.end)
    }
}

#[derive(Serialize, Deserialize)]
#[serde(remote = "SpanAnchor")]
pub struct SpanAnchorDef {
    #[serde(with = "EditionedFileIdDef")]
    pub file_id: EditionedFileId,
    #[serde(with = "ErasedFileAstIdDef")]
    pub ast_id: ErasedFileAstId,
}

#[derive(Serialize, Deserialize)]
#[serde(remote = "TextSize")]
pub struct TextSizeDef {
    #[serde(getter = "text_size_get_raw")]
    raw: u32,
}

fn text_size_get_raw(text_size: &TextSize) -> u32 {
    text_size.to_owned().into()
}

impl From<TextSizeDef> for TextSize {
    fn from(value: TextSizeDef) -> Self {
        TextSize::new(value.raw)
    }
}

#[derive(Serialize, Deserialize)]
#[serde(remote = "EditionedFileId")]
pub struct EditionedFileIdDef(#[serde(getter = "editioned_file_id_get_inner")] u32);

fn editioned_file_id_get_inner(value: &EditionedFileId) -> u32 {
    value.as_u32()
}

impl From<EditionedFileIdDef> for EditionedFileId {
    fn from(value: EditionedFileIdDef) -> Self {
        EditionedFileId::from_raw(value.0)
    }
}

#[derive(Serialize, Deserialize)]
#[serde(remote = "ErasedFileAstId")]
pub struct ErasedFileAstIdDef(#[serde(getter = "erased_file_ast_id_get_inner")] u32);

fn erased_file_ast_id_get_inner(value: &ErasedFileAstId) -> u32 {
    value.into_raw()
}

impl From<ErasedFileAstIdDef> for ErasedFileAstId {
    fn from(value: ErasedFileAstIdDef) -> Self {
        ErasedFileAstId::from_raw(value.0)
    }
}

#[derive(Serialize, Deserialize, Clone, Copy)]
pub struct SyntaxContextDef(
    #[serde(with = "salsa_id_def")] salsa::Id,
    #[serde(skip)]
    std::marker::PhantomData<&'static salsa::plumbing::interned::Value<SyntaxContext>>,
);

impl From<SyntaxContextDef> for SyntaxContext {
    fn from(value: SyntaxContextDef) -> Self {
        SyntaxContext::from_id(value.0)
    }
}

impl From<SyntaxContext> for SyntaxContextDef {
    fn from(value: SyntaxContext) -> Self {
        SyntaxContextDef(
            salsa::Id::from_u32(value.into_u32()),
            std::marker::PhantomData,
        )
    }
}

/// As of writing, salsa::Id does the round-trip to u32 and back correctly.
/// It would be strange for that to break.
mod salsa_id_def {
    use serde::{Deserialize, Deserializer, Serialize, Serializer};

    pub(super) fn serialize<S: Serializer>(
        value: &salsa::Id,
        serializer: S,
    ) -> Result<S::Ok, S::Error> {
        value.as_u32().serialize(serializer)
    }

    pub(super) fn deserialize<'de, D: Deserializer<'de>>(
        deserializer: D,
    ) -> Result<salsa::Id, D::Error> {
        let x = u32::deserialize(deserializer)?;
        Ok(salsa::Id::from_u32(x))
    }
}
