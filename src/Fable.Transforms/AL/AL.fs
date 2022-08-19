namespace rec Fable.AST.AL

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
    //| Sequence of ALExpression * ALExpression unnecessarily complex to debug
    | Assignment of target: ALExpression * source: ALExpression
    | Expression of exp: ALExpression
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
    // | NamedSimpleType of typename:string
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

type ALObject =
    { ObjectId: int
      ObjectType: string
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



