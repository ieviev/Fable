namespace rec Fable.AST.AL

open Fable.AST.AL


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

type ALStatement =
    | Block of ALStatement list
    | Sequence of ALStatement * ALStatement
    | Assignment of target: ALExpression * source: ALExpression
    | Expression of exp: ALExpression
    | IfStatement of guard: ALExpression * ``then``: ALStatement * ``else``: ALStatement option
    // TODO : case, while, repeat, for, with(?), foreach
    | ForLoop of
        lambdaIdentifier: ALExpression *
        assignment: ALExpression *
        exp: ALExpression *
        doStatement: ALStatement *
        isUp: bool
    | ForeachLoop of identifier: ALExpression * exp: ALExpression * doStatement: ALExpression
    | WhileLoop of guard: ALExpression * doStatement: ALExpression
    | Exit of value: ALExpression

type FSALMappingExpr =
    | Ignore
    | StatementExpr of ALStatement
    | DecisionExpr of index: int
    | InvocationWithoutTarget of methodnameAndArgs: (string * ALExpression list)
    | InvocationWithoutLastArg of targetMethodNameAndArgs: (ALExpression * string * ALExpression list)

type ALExpression =
    // these are just for mapping F# code
    | FSALExpr of FSALMappingExpr
    //
    | Constant of obj
    | Identifier of string
    | UnaryExp of op: ALUnaryOperator * right: ALExpression
    | Binary of left: ALExpression * op: ALBinaryOperator * right: ALExpression
    | NaryExpression of expr: ALNaryExpression
    | FSharpLambda of newVariable: ALVariable * ALExpression


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
      LocalVariables: ALVariable list
      Statements: ALStatement
      ReturnType: ALType option
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
