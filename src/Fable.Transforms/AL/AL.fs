namespace rec Fable.AST.AL

open Fable.AST
open Fable.AST.AL
open Fable.AST.Fable


type ALBinaryOperator =
    | Add
    | Subtract
    | Multiply
    | Divide
    | Mod
    | AND
    | OR
    | XOR
    | LessThan
    | LessThanOrEqual
    | Equals
    | GreaterThan
    | GreaterThanOrEqual
    | NotEquals
    | MemberAccess //Exp.Exp
    | Range // Exp..Exp
    | Scope // Exp::Exp

type ALUnaryOperator =
    | UnaryMinus // - Exp
    | Not // NOT Exp
    | Grouping //(Exp)

type ALExpression =
    // these are just for mapping F# code
    | FSALIgnore
    | FSALDecisionExpr of index: int
    | FSALInvocationWithoutTarget of methodnameAndArgs: (string * ALExpression list)
    | FSALInvocationWithoutLastArg of targetMethodNameAndArgs: (ALExpression * string * ALExpression list)
    | FSALLambda of newVariable: ALVariable * ALExpression
    // expressions
    | Constant of obj
    | Identifier of Ident
    | UnaryExp of op: ALUnaryOperator * right: ALExpression
    | Binary of  op: ALBinaryOperator * left: ALExpression * right: ALExpression
    | NaryExpression of expr: ALNaryExpression
    // actually statements
    | Block of ALExpression list
    | Assignment of target: ALExpression * source: ALExpression
    | IfStatement of guard: ALExpression * ``then``: ALExpression * ``else``: ALExpression option
    | ForLoop of
        lambdaIdentifier: ALExpression *
        assignment: ALExpression *
        exp: ALExpression *
        doStatement: ALExpression *
        isUp: bool
    | ForeachLoop of identifier: ALExpression * exp: ALExpression * doStatement: ALExpression
    | WhileLoop of guard: ALExpression * doStatement: ALExpression
    | Exit of value: ALExpression

type ALNaryExpression =
    | Invocation of target: ALExpression * args: ALExpression list
    | ArrayReference of target: ALExpression * args: ALExpression list
    | MembershipTest of target: ALExpression * args: ALExpression list

type ALVariable =
    { IsMutable: bool
      Name: string
      ALType: ALType }

type ALSimpleType =
    | NoneType
    | Boolean
    | Char
    | Integer
    | Decimal
    | DateTime
    | Text of length: int option
    | Code of length: int option
    | Option of values: string list
    | List of listArg: ALSimpleType
    | JsonToken
    | JsonArray
    | JsonObject

type ALComplexType =
    | ComplexType of typename: string
    | Record of name: string
    | Page of name: string
    | Report of name: string
    | Codeunit of name: string
    | Query of name: string
    | Variant

type ALType =
    | Simple of ALSimpleType
    | Complex of ALComplexType

type ALProcedure =
    { IsLocal: bool
      Identifier: string
      Parameters: ALVariable list
      LocalVariables: Set<Ident>
      Statements: ALExpression list
      ReturnType: Fable.AST.Fable.Type // ALType option
     }

type ALRecordField =
    { Id: int
      Identifier: string
      ALType: ALType
      Length: int option
      Properties: (string * obj) list }


type ALCodeunitProperty =
    | SingleInstance of bool

type ALObjectProperty =
    | CodeunitProperty of ALCodeunitProperty

type ALObject =
    { ObjectId: int
      ObjectType: ALComplexType
      Properties: ALObjectProperty list
      Members: ALProcedure list
      Fields: ALRecordField list }

// todo: proper impl
type ALDecl =
    | ALMemberDecl of ALProcedure
    | ALObjectDecl of ALObject

type ALFile =
    { Filename: string
      Namespace: string option
      Require: (string option * string) list
      // todo: uses
      Uses: string list
      //      Decls: (int * PhpDecl) list
      Decls: (int * ALDecl) list }



/////
///
/// ALGEN
///


module ALType =
    type private t = ALType
    
    let rec ofFableType (ftype:Fable.AST.Fable.Type) =
        match ftype with
        | Type.Number(numberKind, numberInfo) ->
            match numberKind with
            | Int32 -> Simple Integer
            | _ -> failwith $"unimplemented type casts {ftype}"
        | _ -> failwith $"unimplemented type casts {ftype}" 
//        match fullname with
//        | "Fs.AL.Core.ALSimpleValues.ALJsonObject" -> Simple JsonObject
//        | "Fs.AL.Core.ALSimpleValues.ALJsonToken" -> Simple JsonToken
//        | "Fable.JsonProvider.Generator<...>" -> Simple JsonToken
//        | Operators.ref -> ftype.GenericArguments[0] |> ofFSharpType
//        | "Microsoft.FSharp.Core.byref" -> ftype.GenericArguments[0] |> ofFSharpType
//        | "Microsoft.FSharp.Core.Ref" -> ftype.GenericArguments[0] |> ofFSharpType
//        // array types
//        | "Microsoft.FSharp.Core.[]" ->
//            let arrayof = ftype.GenericArguments[0] |> ofFSharpType
//            match arrayof with
//            | Simple JsonToken -> Simple JsonArray
//            | Simple (Char) -> Simple (ALSimpleType.List (ALSimpleType.Char))
//            | Simple (Integer) -> Simple (ALSimpleType.List (ALSimpleType.Integer))
//            | Simple (Decimal) -> Simple (ALSimpleType.List (ALSimpleType.Decimal))
//            | Simple (Text n) -> Simple (ALSimpleType.List (ALSimpleType.Text n))
//            | _ -> failwith "unimplemented"
//        | "System.Collections.Generic.List" ->
//            let listof = ftype.GenericArguments[0] |> ofFSharpType
//            match listof with
//            | Simple JsonToken -> Simple JsonArray
//            | Simple (st) -> Simple (List st)
//            | x -> failwith $"invalid AL list, use an array? %A{x}"
//        | "System.Collections.Generic.List`1.Enumerator" ->
//            // todo: create seq type
//            let listof = ftype.GenericArguments[0] |> ofFSharpType
//            match listof with
//            | Simple JsonToken -> Simple JsonArray
//            | Simple (st) -> Simple (List st)
//            | x -> failwith $"invalid AL list, use an array? %A{x}"
//        // types inherited from type system
//        | x when ftype |> FSharpType.hasBaseType<ALCode> -> Simple (Code None)
//        | x when ftype |> FSharpType.hasBaseType<ALCodeunit> ->
//            let name = ftype.TypeDefinition |> FSharpEntity.getALCompiledName
//            Complex (Codeunit name)
//        
//        | x when ftype |> FSharpType.hasBaseType<ALRecord> ->
//            let name = ftype.TypeDefinition |> FSharpEntity.getALCompiledName
//            Complex (Record name)
//        | x when ftype |> FSharpType.hasBaseType<ALSimpleValue> -> Simple (SimpleType (FSharpEntity.getALCompiledName ftype.TypeDefinition))
//        | x when ftype |> FSharpType.hasBaseType<ALComplexValue> -> Complex (ComplexType (FSharpEntity.getALCompiledName ftype.TypeDefinition))
//        | x when ftype.TypeDefinition |> FSharpEntity.hasAttribute<AL.Json> -> Simple JsonToken // use as json type
//        | x when ftype.TypeDefinition |> FSharpEntity.hasAttribute<AL.Option> ->
//            // cast cases to option types
//            let cases =
//                ftype.TypeDefinition.UnionCases
//                |> Seq.map (fun f -> f.Name)
//                |> Seq.toList
//            Simple (Option cases) 

    let quotechars = ".+-$&[]/\\*\"`' " |> Seq.toArray
    let toString (x:t) =
        let d = 5
        match x with
        | Complex t ->
            match t with
            | ComplexType typename -> typename
            | Variant -> nameof Variant
            | Record name -> name
            | Codeunit name -> name
            | x ->
                let d = 5
                failwithf $"%A{x}"
        | Simple t ->
            match t with
            | Text lenOpt -> nameof Text
            | DateTime -> nameof DateTime
            | Integer -> nameof Integer
            | Boolean -> nameof Boolean
            | Decimal -> nameof Decimal
            | Code lenOpt -> nameof Code
            | JsonToken -> nameof JsonToken
            | JsonArray -> nameof JsonArray
            | JsonObject -> nameof JsonObject
            | List (listparam) ->
                "ListOf:" + listparam.ToString()
            | Option cases ->
                "OptionOf:" + cases.ToString()
            | x ->
                failwithf $"%A{x}"