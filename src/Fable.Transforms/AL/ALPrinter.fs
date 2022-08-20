module Fable.Transforms.ALPrinter

open System
open Fable.AST.AL
open Fable.Transforms.AL

module Output =
    type Writer =
        { Writer: System.Text.StringBuilder
          Indent: int
          Precedence: int
          UsedTypes: string Set
          CurrentNamespace: string option }

    let indent ctx =
        { ctx with Indent = ctx.Indent + 1}

    module Writer =
        let create w =
            { Writer = w; Indent = 0; Precedence = Int32.MaxValue; UsedTypes = Set.empty; CurrentNamespace = None }

    let writeIndent ctx =
        for _ in 1 .. ctx.Indent do
            ctx.Writer.Append("    ") |> ignore

    let write ctx txt =
        ctx.Writer.Append(txt: string) |> ignore

    let writeln ctx txt =
        ctx.Writer.AppendLine(txt: string) |> ignore

    let writei ctx txt =
        writeIndent ctx
        write ctx txt

    let writeiln ctx txt =
        writeIndent ctx
        writeln ctx txt

    let writeStr ctx (str: string) =
        write ctx "'"
        write ctx (str.Replace(@"\",@"\\").Replace("'",@"\'"))
        write ctx "'"

    let writeFile ctx (file: ALFile) =
        writeln ctx "// fbl"
        let ctx =
            { ctx with
                UsedTypes = set file.Uses
                CurrentNamespace = file.Namespace }
        for i,d in file.Decls do
//            writeln ctx ( "//" + string i)
            match d with
            | ALObjectDecl alObject ->
                let codeunitString =
                    ALGen.Object.createALCodeunit alObject
                writeln ctx codeunitString
            | _ -> failwith "unimplemented" 
            writeln ctx ""


let isEmpty (file: ALFile): bool =
    false //TODO: determine if printer will not print anything

let run (writer: Printer.Writer) (file: ALFile): Async<unit> =
    async {
        let sb = System.Text.StringBuilder()
        let ctx = Output.Writer.create sb
        Output.writeFile ctx file
        do! writer.Write(sb.ToString())
    }
