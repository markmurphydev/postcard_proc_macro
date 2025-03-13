// //! Non-flattened versions of `legacy_protocol/msg.rs` data structures.

// #[derive(Debug, Serialize, Deserialize)]
// pub struct JsonExpandMacroData {
//     pub macro_body: Subtree,
//     pub macro_name: String,
//     pub attributes: Option<Subtree>,

//     #[serde(skip_serializing_if = "ExpnGlobals::skip_serializing_if")]
//     #[serde(default)]
//     pub has_global_spans: ExpnGlobals,
//     /// Table of additional span data.
//     #[serde(skip_serializing_if = "Vec::is_empty")]
//     #[serde(default)]
//     pub span_data_table: Vec<u32>,
// }
