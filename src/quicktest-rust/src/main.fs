// [<Fable.Core.Erase; Fable.Core.Rust.OuterAttr("cfg", [| "target_arch = \"wasm32\"" |])>]
module rust_wasm

open Fable.Core
open Fable.Core.Rust


type StringOrInt =
    | String of string
    | Int of uint


type Expr =
    | Literal of text: string
    | Regex of pattern: string * id: StringOrInt option
    | Reference of name: string
    | Concat of ResizeArray<Expr>
    | Union of left: Expr * right: Expr
    | Inter of left: Expr * right: Expr
    | Compl of right: Expr


type Decl = | LetBinding of attrs: string[] * name: string * expr: Expr

type Model = { decls: Decl[] }

[<Erase; Emit("&$0")>]
let inline ref(x: 't) : Ref<'t> = nativeOnly

[<Erase; Struct; Emit("Vec<$0>")>]
type Vec<'u8> =
    struct
    end

    [<Emit("Vec::new()")>]
    static member new_() : Vec<'u8> = nativeOnly

    [<Emit("$0.push($1)")>]
    member _.push() : unit = nativeOnly

    [<Emit("$0.len()")>]
    member inline _.len() : unativeint = nativeOnly

    [<Emit("$0[$1 as usize]")>]
    member inline _.Item(_: unativeint) : 'u8 = nativeOnly

    static member inline Iterate(arg: Vec<'u8>, [<Emit("fn($0)")>] fn: 'u8 -> unit) : unit =
        let mutable i: unativeint = 0un
        let endlen = arg.len ()

        while i < endlen do
            fn (arg[i])
            i <- i + 1un


let dummy() : unit =
    import "wasm_bindgen::prelude::*" ""
    ()

[<Erase; Emit("resharp_algorithm::NodeId")>]
type NodeId =
    interface
        [<Emit("resharp_algorithm::NodeId::TOPSTAR")>]
        static abstract member ts: NodeId
    end

[<Erase; Emit("resharp_algorithm::Resharp")>]
type Resharp =
    [<Emit("$0.b.mk_concat($1)")>]
    abstract member mk_concat: node: NodeId -> unit

let inline emit(s: string) = RustInterop.emitRustExpr "" s

let compile_decl(d: byref<Decl>) =
    let mutable resharp: Resharp = emit "resharp_algorithm::Resharp::new()"


    match d with
    | LetBinding(_, name, _) -> print (name)


    // let asd = resharp.mk_concat (NodeId.ts)
    ()

// [<Rust.OuterAttr("wasm_bindgen")>]
// let generate_impl(ast_string: _str) =
//     // let decls: Vec<Decl> =
//     //     emit "crate::helpers::parsing::parse_slice(ast_string.as_bytes())"

//     // let compiled = compile_decl (ref decls[0un])

//     1
