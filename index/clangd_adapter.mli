(** [ingest clangd_index_file_yaml] returns all function signatures present in [clangd_index_file_yaml],
which should be produced by clang's [clangd-indexer] tool with --format=yaml

Malformed or non-function entries are ignored
*)
val ingest : string -> Signature.CFunction.t list
