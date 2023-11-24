// this is how the gluon project imported the generated grammar
//
// even though its a bit more brittel I like it more than using the the lalrpop_mod! macro.
// I feel like its more clear whats going on.
include!(concat!(env!("OUT_DIR"), "/parser/grammar.rs"));
