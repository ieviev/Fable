module Fable.Transforms.ALExpr

open Fable.AST.AL
open Fable.AST

let identifier (ident:Fable.Ident) =
    ALExpression.Identifier ident

let assignmentIdent (ident:Fable.Ident) source =
    let identId = ALExpression.Identifier ident
    match source with
    | [ x1 ] -> ALExpression.Assignment(identId,x1)
    | other -> failwith $"unable to assign from {other}"

let binary binop left right =
    Binary(binop,left,right)