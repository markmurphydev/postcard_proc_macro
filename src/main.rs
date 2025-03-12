use std::{collections::HashMap, env, error::Error, path::Path};

use intern::{Symbol, sym};
use paths::{AbsPath, AbsPathBuf, Utf8Path, Utf8PathBuf};
use postcard_proc_macro::{MacroDylib, ProcMacroClient};
use span::{
    Edition, EditionedFileId, ErasedFileAstId, FileId, ROOT_ERASED_FILE_AST_ID, Span, SpanAnchor,
    SpanData, SyntaxContextId,
};
use tt::{
    Delimiter, DelimiterKind, Ident, Leaf, Literal, Punct, Spacing, TextRange, TextSize,
    TopSubtree, TopSubtreeBuilder,
};

fn fixture_token_tree() -> TopSubtree<Span> {
    // colorless green ideas sleep furiously
    let anchor = SpanAnchor {
        file_id: span::EditionedFileId::new(
            span::FileId::from_raw(0xe4e4e),
            span::Edition::CURRENT,
        ),
        ast_id: ErasedFileAstId::from_raw(0),
    };

    let mut builder = TopSubtreeBuilder::new(Delimiter {
        open: Span {
            range: TextRange::empty(TextSize::new(0)),
            anchor,
            ctx: SyntaxContextId::root(Edition::CURRENT),
        },
        close: Span {
            range: TextRange::empty(TextSize::new(19)),
            anchor,
            ctx: SyntaxContextId::root(Edition::CURRENT),
        },
        kind: DelimiterKind::Invisible,
    });

    builder.push(
        Ident {
            sym: Symbol::intern("struct"),
            span: Span {
                range: TextRange::at(TextSize::new(0), TextSize::of("struct")),
                anchor,
                ctx: SyntaxContextId::root(Edition::CURRENT),
            },
            is_raw: tt::IdentIsRaw::No,
        }
        .into(),
    );
    builder.push(
        Ident {
            sym: Symbol::intern("Foo"),
            span: Span {
                range: TextRange::at(TextSize::new(5), TextSize::of("r#Foo")),
                anchor,
                ctx: SyntaxContextId::root(Edition::CURRENT),
            },
            is_raw: tt::IdentIsRaw::Yes,
        }
        .into(),
    );
    builder.push(Leaf::Literal(Literal {
        symbol: Symbol::intern("Foo"),
        span: Span {
            range: TextRange::at(TextSize::new(10), TextSize::of("\"Foo\"")),
            anchor,
            ctx: SyntaxContextId::root(Edition::CURRENT),
        },
        kind: tt::LitKind::Str,
        suffix: None,
    }));
    builder.push(Leaf::Punct(Punct {
        char: '@',
        span: Span {
            range: TextRange::at(TextSize::new(13), TextSize::of('@')),
            anchor,
            ctx: SyntaxContextId::root(Edition::CURRENT),
        },
        spacing: Spacing::Joint,
    }));
    builder.open(
        DelimiterKind::Brace,
        Span {
            range: TextRange::at(TextSize::new(14), TextSize::of('{')),
            anchor,
            ctx: SyntaxContextId::root(Edition::CURRENT),
        },
    );
    builder.push(Leaf::Literal(Literal {
        symbol: sym::INTEGER_0.clone(),
        span: Span {
            range: TextRange::at(TextSize::new(15), TextSize::of("0u32")),
            anchor,
            ctx: SyntaxContextId::root(Edition::CURRENT),
        },
        kind: tt::LitKind::Integer,
        suffix: Some(sym::u32.clone()),
    }));
    builder.close(Span {
        range: TextRange::at(TextSize::new(19), TextSize::of('}')),
        anchor,
        ctx: SyntaxContextId::root(Edition::CURRENT),
    });

    builder.build()
}

fn main() -> Result<(), anyhow::Error> {
    let token_tree = fixture_token_tree();
    println!(
        "token_tree={}",
        tt::pretty(token_tree.view().as_token_trees().flat_tokens())
    );

    let cwd = env::current_dir().unwrap();
    let cwd = Utf8Path::from_path(&cwd).unwrap();
    let cwd = AbsPath::assert(cwd);

    let serde_derive_path = "./target/debug/deps/libsimple_define_macro-07a714d74ad901f3.dylib";
    let serde_derive_path = Utf8Path::new(serde_derive_path);
    let serde_derive_path = cwd.absolutize(serde_derive_path);
    // let serde_derive_path = AbsPath::assert(serde_derive_path);

    let proc_macro_path = AbsPathBuf::assert(Utf8PathBuf::from(
        "/Users/boathouse/.rustup/toolchains/stable-aarch64-apple-darwin/libexec/rust-analyzer-proc-macro-srv",
    ));

    // println!("path={serde_derive_path}");

    let env: HashMap<String, String> = HashMap::new();

    let client = ProcMacroClient::spawn(&proc_macro_path, env.clone())?;
    println!("client={client:?}");
    let derive_macros = client
        .load_dylib(MacroDylib::new(serde_derive_path))
        .unwrap();
    println!("derive_macros={derive_macros:#?}");

    let simple_define_macro = derive_macros[0].clone();

    let file_id = FileId::from_raw(22);

    let def_site = SpanData {
        range: TextRange::new(TextSize::new(0), TextSize::new(0)),
        anchor: SpanAnchor {
            file_id: EditionedFileId::current_edition(file_id),
            ast_id: ROOT_ERASED_FILE_AST_ID,
        },
        ctx: SyntaxContextId::root(Edition::CURRENT),
    };
    let call_site = def_site.clone();
    let mixed_site = def_site.clone();
    let res = simple_define_macro
        .expand(
            token_tree.view(),
            None,
            env.into_iter().collect(),
            def_site,
            call_site,
            mixed_site,
            None,
        )
        .expect("EXPAND");
    println!("res={res:?}");

    Ok(())
}
