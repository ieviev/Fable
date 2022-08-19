module Fable.Transforms.ALHelpers

open Fable.Core
open Fable.AST

[<RequireQualifiedAccess>]
module Literals =
    
    [<RequireQualifiedAccess>]
    module Atts = 
        let [<Literal>] ``Name`` = """Fable.Core.AL.Name""" 
        let [<Literal>] ``Codeunit`` = """Fable.Core.AL.Codeunit""" 
        let [<Literal>] ``Table`` = """Fable.Core.AL.Table"""
        let [<Literal>] ``Json`` = """Fable.Core.AL.Json"""
        let [<Literal>] ``Option`` = """Fable.Core.AL.Option"""


[<RequireQualifiedAccess>]
module Entity =
    
    let hasAttribute fullName (ent: Fable.Entity) =
        ent.Attributes |> Seq.exists (fun att -> att.Entity.FullName = fullName)
    let hasInterface fullName (ent: Fable.Entity) =
        ent |> FSharp2Fable.Util.hasInterface fullName
