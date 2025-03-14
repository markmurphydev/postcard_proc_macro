use std::{collections::HashMap, env};

use intern::{Symbol, sym};
use paths::{AbsPath, AbsPathBuf, Utf8Path, Utf8PathBuf};
use postcard_proc_macro::{
    MacroDylib, ProcMacroClient,
    legacy_protocol::msg::{
        ExpandMacro, ExpandMacroData, ExpnGlobals, FlatTree, HAS_GLOBAL_SPANS,
        RUST_ANALYZER_SPAN_SUPPORT, Request, SpanDataIndexMap, serialize_span_data_index_map,
    },
    process::ProcMacroServerProcess,
    serde_derives::TopSubtreeSerdeHelper,
};
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
    let top_subtree: TopSubtree<Span> = fixture_token_tree();
    // println!(
    //     "token_tree={}",
    //     tt::pretty(token_tree.view().as_token_trees().flat_tokens())
    // );

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
    // println!("client={client:?}");
    let derive_macros = client
        .load_dylib(MacroDylib::new(serde_derive_path))
        .unwrap();
    // println!("derive_macros={derive_macros:#?}");

    let simple_define_macro = derive_macros[0].clone();

    let file_id = FileId::from_raw(22);

    let def_site: Span = SpanData {
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
            top_subtree.view(),
            None,
            env.into_iter().collect(),
            def_site,
            call_site,
            mixed_site,
            None,
        )
        .expect("EXPAND");
    // println!("res={res:?}");

    let top_subtree_helper: TopSubtreeSerdeHelper = top_subtree.clone().into();
    let json = serde_json::to_string(&top_subtree_helper)?;
    println!(
        "JSON\n\n{}",
        serde_json::to_string_pretty(&top_subtree_helper)?
    );

    let un_json = serde_json::from_str::<TopSubtreeSerdeHelper>(&json)?;
    let un_json: TopSubtree<Span> = un_json.into();

    assert_eq!(top_subtree, un_json);

    Ok(())
}

fn expand_json(
    dylib: MacroDylib,
    server: ProcMacroServerProcess,
    subtree: tt::SubtreeView<'_, Span>,
    attr: Option<tt::SubtreeView<'_, Span>>,
    env: Vec<(String, String)>,
    def_site: Span,
    call_site: Span,
    mixed_site: Span,
    current_dir: Option<String>,
) {
    let version = server.version();

    let mut span_data_table = SpanDataIndexMap::default();
    let def_site = span_data_table.insert_full(def_site).0;
    let call_site = span_data_table.insert_full(call_site).0;
    let mixed_site = span_data_table.insert_full(mixed_site).0;
    let task = ExpandMacro {
        data: ExpandMacroData {
            macro_body: FlatTree::new(subtree, version, &mut span_data_table),
            macro_name: "MACRO_NAME".to_string(),
            attributes: attr.map(|subtree| FlatTree::new(subtree, version, &mut span_data_table)),
            has_global_spans: ExpnGlobals {
                serialize: version >= HAS_GLOBAL_SPANS,
                def_site,
                call_site,
                mixed_site,
            },
            span_data_table: if version >= RUST_ANALYZER_SPAN_SUPPORT {
                serialize_span_data_index_map(&span_data_table)
            } else {
                Vec::new()
            },
        },
        lib: dylib.path.to_path_buf().into(),
        env,
        current_dir,
    };
    let req = Request::ExpandMacro(Box::new(task));
    let response = server.send_task(req);
}
