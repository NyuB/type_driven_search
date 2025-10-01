module SymInfo = struct
  type t =
    { kind : string [@key "Kind"]
    ; lang : string [@key "Lang"]
    }
  [@@deriving yaml]
end

module CanonicalDeclaration = struct
  type source_location =
    { line : int [@key "Line"]
    ; column : int [@key "Column"]
    }
  [@@deriving yaml]

  type t =
    { fileURI : string [@key "FileURI"]
    ; start : source_location [@key "Start"]
    ; end_ : source_location [@key "End"]
    }
  [@@deriving yaml]
end

module IncludeHeader = struct
  type t =
    { header : string [@key "Header"]
    ; references : int [@key "References"]
    }
  [@@deriving yaml]
end

(** Handle an hexadecimal that would be only digits *)
let decode_id = function
  | `String s -> Ok s
  | `Float i ->
    if Float.is_integer i
    then Ok (string_of_int (int_of_float i))
    else Error (`Msg "Expected an id (string) but got a floating point value")
  | _ -> Error (`Msg "Expected an id (string) but got another yaml value")
;;

type symbol =
  { id : string [@key "ID"] [@of_yaml decode_id]
  ; name : string [@key "Name"]
  ; scope : string [@key "Scope"]
  ; symInfo : SymInfo.t [@key "SymInfo"]
  ; canonicalDeclaration : CanonicalDeclaration.t [@key "CanonicalDeclaration"]
  ; flags : int [@key "Flags"]
  ; signature : string [@key "Signature"]
  ; templateSpecializationArgs : string [@key "TemplateSpecializationArgs"]
  ; completionSnippetSuffix : string [@key "CompletionSnippetSuffix"]
  ; documentation : string [@key "Documentation"]
  ; returnType : string [@key "ReturnType"]
  ; type_ : string [@key "Type"]
  ; includeHeaders : IncludeHeader.t list [@key "IncludeHeaders"]
  }
[@@deriving yaml]

let read_clangd_index_file f =
  let rec aux documents current_document ic =
    match input_line ic with
    | line ->
      let current_document = line :: current_document in
      if String.starts_with ~prefix:"..." line
      then aux (String.concat "\n" (List.rev current_document) :: documents) [] ic
      else aux documents current_document ic
    | exception End_of_file -> List.rev documents
  in
  In_channel.with_open_text f @@ fun ic -> aux [] [] ic
;;

let ( >> ) g f = Fun.compose f g

let parser =
  let open Parsers in
  let type_and_opt_param = Signature.Ctype.parser |. whitespaces |. option identifier in
  list
    ~prefix:(keyword "(")
    ~suffix:(keyword ")")
    ~sep:(whitespaces |. keyword ",")
    (whitespaces ||> type_and_opt_param)
;;

let cfunction_of_symbol { name; symInfo = { kind; _ }; signature; returnType; _ } =
  if not @@ String.equal kind "Function"
  then None
  else
    Parsers.parse_full parser signature
    |> Option.map (fun params ->
      let (signature : Signature.t) =
        { params; return = Signature.Ctype.parse returnType }
      in
      ({ name; signature } : Signature.CFunction.t))
;;

let ingest file =
  file
  |> read_clangd_index_file
  |> List.map
       (Yaml.of_string_exn
        >> symbol_of_yaml
        >> function
        | Error (`Msg m) -> failwith m
        | Ok v -> v)
  |> List.filter_map cfunction_of_symbol
;;
