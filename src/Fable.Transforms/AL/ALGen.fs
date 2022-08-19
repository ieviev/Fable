module Fable.Transforms.AL.ALGen

open System.IO
open Microsoft.Dynamics.Nav.CodeAnalysis
open Microsoft.Dynamics.Nav.CodeAnalysis
open Microsoft.Dynamics.Nav.CodeAnalysis.Syntax

type private sf = Microsoft.Dynamics.Nav.CodeAnalysis.SyntaxFactory

module Trivia =
    let _4spc = [|sf.Space;sf.Space;sf.Space;sf.Space|]
    let lf4spc = [|sf.Linefeed;sf.Space;sf.Space;sf.Space;sf.Space|]
    let lf8spc = [|sf.Linefeed;sf.Space;sf.Space;sf.Space;sf.Space;sf.Space;sf.Space;sf.Space;sf.Space|]
    let lf12spc = [|sf.Linefeed;sf.Space;sf.Space;sf.Space;sf.Space;sf.Space;sf.Space;sf.Space;sf.Space;sf.Space;sf.Space;sf.Space;sf.Space|]
    

