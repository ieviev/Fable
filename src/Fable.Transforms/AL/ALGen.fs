[<RequireQualifiedAccess>]
module rec Fable.Transforms.AL.ALGen

open System
open System.Globalization
open System.IO
open Fable.AST.AL
open Microsoft.Dynamics.Nav.CodeAnalysis
open Microsoft.Dynamics.Nav.CodeAnalysis
open Microsoft.Dynamics.Nav.CodeAnalysis.Syntax


type private sf = Microsoft.Dynamics.Nav.CodeAnalysis.SyntaxFactory
type private sk = Microsoft.Dynamics.Nav.CodeAnalysis.SyntaxKind

type NavExpr =
    | Exp of ExpressionSyntax
    | Stat of StatementSyntax


module Trivia =
    let _4spc = [|sf.Space;sf.Space;sf.Space;sf.Space|]
    let lf4spc = [|sf.Linefeed;sf.Space;sf.Space;sf.Space;sf.Space|]
    let lf8spc = [|sf.Linefeed;sf.Space;sf.Space;sf.Space;sf.Space;sf.Space;sf.Space;sf.Space;sf.Space|]
    let lf12spc = [|sf.Linefeed;sf.Space;sf.Space;sf.Space;sf.Space;sf.Space;sf.Space;sf.Space;sf.Space;sf.Space;sf.Space;sf.Space;sf.Space|]
    
module Extensions = 
    type SyntaxNode with
        member x.lf12s() = downcast x.WithLeadingTrivia(Trivia.lf12spc)
        member x.lf4s() = downcast x.WithLeadingTrivia(Trivia.lf4spc)
        static member wls (token:_) = token.WithLeadingTrivia(sf.Space)
    
        

module SeparatedSyntaxList =
    
    let ofDataTypeSyntax (x:DataTypeSyntax) =
        let ssl = SeparatedSyntaxList()
        ssl.Add(x)
        
    let ofValues (values:string list) =
        let ssl = SeparatedSyntaxList()
        List.fold (fun (acc:SeparatedSyntaxList<IdentifierNameOrEmptySyntax>) (f:string) ->
            f |> sf.IdentifierName |> sf.IdentifierNameOrEmpty |> acc.Add )
            ssl values
            
            
//// NODES
///
///

module PropertyList =
    open Fable.Transforms.AL.ALGen.Extensions
    let create (props:PropertySyntaxOrEmpty seq) =
        sf.PropertyList(
            sf.List(props))
        
    let ofOptionValues (values:string list) : PropertySyntaxOrEmpty seq =
        let proplist = 
            seq {
                let optmempropvalue =
                    values
                    |> SeparatedSyntaxList.ofValues
                    |> sf.OptionValues
                    |> sf.OptionValuesPropertyValue
                yield sf.Property(PropertyKind.OptionMembers, optmempropvalue).lf12s() :> PropertySyntaxOrEmpty
                let optcaptpropvalue =
                    values
                    |> String.concat ","
                    |> sf.Literal
                    |> sf.StringLiteralValue
                    |> sf.StringPropertyValue
                yield sf.Property(PropertyKind.OptionCaption,optcaptpropvalue).lf12s() :> PropertySyntaxOrEmpty
            }    
        proplist
 
    
    let ofALObjectProperties (props:ALObjectProperty list) =
        let rec loop acc (list:ALObjectProperty list) =
            match list with
            | [] -> acc
            | head :: tail ->
                let prop =
                    match head with
                    | CodeunitProperty (SingleInstance b) ->
                        sf.PropertyLiteral(PropertyKind.SingleInstance,true).lf4s()
                loop (prop::acc) tail
                
        let props' = loop [] props
        props'
        |> sf.List
        |> sf.PropertyList
        |> (fun f -> f.WithTrailingTrivia(sf.Linefeed))

module VariableDeclaration =
    let create (name:string) (typeref:TypeReferenceBaseSyntax) =
        let d = 5
        sf.VariableDeclaration(name,typeref)
        :> VariableDeclarationBaseSyntax
        |> (fun f -> f.WithLeadingTrivia(Trivia.lf8spc))
        
        
module VarSection =
    let create declarations = sf.VarSection(declarations).WithLeadingTrivia(Trivia.lf4spc)
    
module ParameterList =
    let withSemicolons (x:ParameterListSyntax) =
        let separators = x.Parameters.GetSeparators()
        let newtoken = sf.Token(SyntaxKind.SemicolonToken).WithTrailingTrivia(sf.Space)
        x.ReplaceTokens(separators,(fun a b -> newtoken ))
        
    let create (parameters:ALVariable list) =
        let pm = 5
        parameters
        |> Seq.map (fun f ->
            let p = sf.Parameter(f.Name,f.ALType |> TypeReference.create)
            if f.IsMutable then
                p.WithVarKeyword(sf.Token(sk.VarKeyword).WithTrailingTrivia(sf.Space))
            else p                
        )
        |> SeparatedSyntaxList().AddRange
        |> sf.ParameterList
        |> withSemicolons
 
 
module TypeReference =
    
    let create (name:ALType) : TypeReferenceBaseSyntax =
        match name with
        | Simple (Option values) ->
            let opttype =
                sf.ParseToken("Option") |> sf.OptionDataType
            let vals = SeparatedSyntaxList.ofValues values
            opttype.WithOptionValues(sf.OptionValues vals)
            |> sf.SimpleTypeReference
            :> TypeReferenceBaseSyntax
        | Simple (List lt) ->
            let inner =
                Simple lt
                |> ALType.toString
                |> sf.ParseToken
    //            |> sf.ObjectNameOrId
                |> sf.SimpleNamedDataType
            
            let okt = sf.Token(sk.OfKeyword).WithTrailingTrivia(sf.Space) 
            let opb = sf.Token(sk.OpenBracketToken)
            let closebrace = sf.Token(sk.CloseBracketToken) 
            let typename = sf.ParseToken("List").WithLeadingTrivia(sf.Space).WithTrailingTrivia(sf.Space)
            let listt =
                sf.GenericNamedDataType(typename)
                    .WithOfKeyword(okt)
                    .WithOpenBracketToken(opb)
                    .WithCloseBracketToken(closebrace)
                    .WithTypeArguments(SeparatedSyntaxList.ofDataTypeSyntax inner)
            listt
            |> sf.SimpleTypeReference
            :> TypeReferenceBaseSyntax
        | Complex (Record name) ->
            let a = 5
//            let recname = name |> sf.ParseToken
            let coe = sf.IdentifierName $"{name}"
            let objnameorid = sf.ObjectNameOrId(coe)
            let reckeyword = sf.ParseToken("Record").WithTrailingTrivia(sf.Space).WithLeadingTrivia(sf.Space)
//            let reckeyword = sf.Token(sk.RecordTypeReference).WithTrailingTrivia(sf.Space)
            let st = sf.SubtypedDataType(reckeyword,objnameorid)
            let str = sf.RecordTypeReference(st)
            str :> TypeReferenceBaseSyntax
        | Complex (Codeunit name) ->
            let coe = sf.IdentifierName $"{name}"
            let objnameorid = sf.ObjectNameOrId(coe)
            let reckeyword = sf.ParseToken("Codeunit").WithTrailingTrivia(sf.Space).WithLeadingTrivia(sf.Space)
            let st = sf.SubtypedDataType(reckeyword,objnameorid)
            sf.SimpleTypeReference(st)
            |> (fun f -> f.WithLeadingTrivia(sf.Space))
        | Simple _ ->
            name
            |> ALType.toString
            |> sf.ParseToken
            |> sf.SimpleNamedDataType
            |> sf.SimpleTypeReference
            |> (fun f -> f.WithLeadingTrivia(sf.Space))
        | Complex _ ->
            name
            |> ALType.toString
            |> sf.ParseToken
            |> sf.SimpleNamedDataType
            |> sf.SimpleTypeReference
            |> (fun f -> f.WithLeadingTrivia(sf.Space))

module Block =
    let ofStatements level (statements:NavExpr list) =
        let getleveltrivia n = [| for i = 0 to n do Trivia._4spc |] |> Array.concat
        let blocklevel = Array.append Trivia.lf4spc (getleveltrivia level)
        let innerLevel = Array.append Trivia.lf4spc (getleveltrivia (level + 1))
        
        let statements =
            statements
            |> List.map (fun f ->
                match f with
                | Stat statementSyntax -> statementSyntax     
                | Exp expressionSyntax ->
                    sf.ExpressionStatement(expressionSyntax :?> CodeExpressionSyntax).WithLeadingTrivia(innerLevel)  
            ) 
        let statement =
            sf.Block(statements |> sf.List)
                .WithBeginKeywordToken(
                    sf.Token(sk.BeginKeyword).WithLeadingTrivia(blocklevel))
                .WithEndKeywordToken(sf.Token(sk.EndKeyword).WithLeadingTrivia(blocklevel))
                .WithSemicolonToken(sf.Token(sk.SemicolonToken))
        statement

module Procedure =
    
    let create isLocal (name:string) parameters variables (body:BlockSyntax) returnval =
        let returnv =
            match returnval with
            | None -> null
            | Some value ->
                sf.ReturnValue(TypeReference.create value)
            
        let nametoken = sf.IdentifierName(name).WithLeadingTrivia(sf.Space)
        sf.MethodDeclaration(nametoken)
            .WithBody(body)
            .WithParameterList(parameters)
            .WithVariables(variables)
            .WithReturnValue(returnv.WithLeadingTrivia(sf.Space))
            .WithAccessModifier(
                match isLocal with   
                | true -> sf.Token(sk.LocalKeyword).WithTrailingTrivia(sf.Space)
                | _ -> sf.Token(sk.EmptyToken)
            )
            |> (fun f -> f.WithLeadingTrivia(Trivia.lf4spc))

     
module Members =
    let createMember (proc:ALProcedure) : MemberSyntax =
        
        let variables =
            proc.LocalVariables
            |> Seq.map (fun f ->
                let typ = f.Type |> ALType.ofFableType
                let tr = (TypeReference.create typ)
                VariableDeclaration.create f.Name tr
            )
            |> sf.List
            |> VarSection.create
         
        let statements =
            proc.Statements
            |> List.collect (NavCode.transformExpression 0)
            
        let parameters = proc.Parameters |> ParameterList.create
        let procbody = Block.ofStatements -1 statements |> (fun f -> f.WithTrailingTrivia(sf.Linefeed))
        let retType = proc.ReturnType |> ALType.ofFableType |> Some
        Procedure.create proc.IsLocal proc.Identifier parameters variables procbody retType
   
//    let createField (builder:ALFieldBuilder) : FieldSyntax =
//        match builder with
//        | RecordField b ->
//            Field.create b.id b.identifier b.altype  
      
      
module Codeunit =
    let create (id:int) (name:string) =
        let objId = sf.ObjectId(id).WithLeadingTrivia(sf.Space)
        let objName = sf.IdentifierName(name).WithLeadingTrivia(sf.Space).WithTrailingTrivia(sf.Linefeed)
        let cu = sf.Codeunit(objId,objName)
        cu      
   

module Record =
    let create (id:int) (name:string) =
        let objId = sf.ObjectId(id).WithLeadingTrivia(sf.Space)
        let objName = sf.IdentifierName(name).WithLeadingTrivia(sf.Space).WithTrailingTrivia(sf.Linefeed)
        let cu =
            sf.Table(objId,objName)
            |> (fun f -> f.WithCloseBraceToken(f.CloseBraceToken.WithLeadingTrivia(sf.Linefeed)))
                
        cu   
  
module Field =
    
    
    let create (id:int) (name:string) (altype:ALType) =
        
        let a = 5
        let datatype =
            match altype with
            | Simple (Text (Some len))  
            | Simple (Code (Some len)) ->
                let t = 5
                let tok = altype |> ALType.toString |> sf.ParseToken
                let lt =
                    sf.LengthDataType(tok,sf.ParseToken(len|> string))
                        .WithOpenBracketToken(sf.Token(sk.OpenBracketToken))
                        .WithCloseBracketToken(sf.Token(sk.CloseBracketToken))
                        :> DataTypeSyntax
                lt
            | Simple (Option values) ->
                TypeReference.create (Simple (Option []))
                |> (fun f -> f.DataType)
            | _ ->
                let tr = TypeReference.create altype
                tr.DataType
            | _ ->
                altype |> ALType.toString
                |> sf.ParseToken
                |> sf.SimpleNamedDataType :> DataTypeSyntax
        
        let field =
            sf.Field(id,name,datatype)
            |> (fun f -> f.WithLeadingTrivia(Trivia.lf8spc))
            |> (fun f -> f.WithOpenBraceToken(f.OpenBraceToken.WithLeadingTrivia(Trivia.lf8spc)))
            |> (fun f -> f.WithCloseBraceToken(f.CloseBraceToken.WithLeadingTrivia(Trivia.lf8spc)))
            
        
        let field2 =
            match altype with
            | Simple (Option values) ->
                let proplist = values |> PropertyList.ofOptionValues |> PropertyList.create
                field.WithPropertyList(proplist)
            | _ -> field
                
        field2  
  
module FieldList =
    
    let create (fields:FieldSyntax seq) =
        
        let list =
            fields
            |> sf.List
            |> sf.FieldList
            |> (fun f -> f.WithLeadingTrivia(Trivia.lf4spc))
            |> (fun f -> f.WithOpenBraceToken(f.OpenBraceToken.WithLeadingTrivia(Trivia.lf4spc)))
            |> (fun f -> f.WithCloseBraceToken(f.CloseBraceToken.WithLeadingTrivia(Trivia.lf4spc)))
                
        
        list  
      
module Object =
    let createALCodeunit (alObject:ALObject) =
        
        let props = PropertyList.ofALObjectProperties alObject.Properties
        let name =
            match alObject.ObjectType with
            | Codeunit name -> name 
            | _ ->  failwith "invalid codeunit"
        
        let filename = $"Codeunit.{alObject.ObjectId}.{name}.al"
        let navCodeunit = Codeunit.create alObject.ObjectId name
            
        let members =
            alObject.Members
            |> List.map Members.createMember
            
        let ALCode =
            navCodeunit
                .WithMembers(sf.List(members))
                .WithPropertyList(props)
                .ToFullString()
        
//        let writeoutput() =
//            Path.Combine(builder.sharedCache.outputPath,filename)
//            |> (fun f -> File.WriteAllText(f,ALCode))
            
        printfn $"compiled %s{filename}"
        ALCode
        
        
/// Expressions
///

module ALGenExpr =
    
    let chooseStat l  = l |> List.choose (fun f -> match f with | Stat s -> Some s | _ -> None)
    let chooseExp l  = l |> List.choose (fun f -> match f with | Exp s -> Some s | _ -> None)
    let toExp l  = match l with | Exp s ->s | _ -> failwith ""   
    let toStat l  = match l with | Stat s ->s | _ -> failwith ""
    let castStat l  = l :> StatementSyntax |> Stat
        
    
    let toCodeExpression lev s =
        NavCode.transformExpression lev s
        |> List.head
        |> function 
        | Exp e -> e
        | _ -> failwith "invalid exp"
        :?> CodeExpressionSyntax

    let ofSingle n =
        n |> Exp |> List.singleton
        
    let toSingleCodeExpression (list:NavExpr list) =
        list
        |> chooseExp
        |> List.head
        :?> CodeExpressionSyntax
        
    let assignment (level:int) ((assignedTo:ALExpression),(expression:ALExpression)) =
        let leveltrivia = [| for i = 0 to level do Trivia._4spc |] |> Array.concat
        let keywordLeading = Array.append Trivia.lf4spc leveltrivia
        // todo: unwrap assignment trees
        let target =
            assignedTo
            |> NavCode.transformExpression level
            |> toSingleCodeExpression
//        let tg2 = |> toCodeExpression level
        let assignedvalue =
            match expression with
            | _ -> expression |> NavCode.transformExpression level |> toSingleCodeExpression
        let assignStatement =
            sf.AssignmentStatement(target,assignedvalue)
                .WithAssignmentToken(sf.Token(sk.AssignToken).WithLeadingTrivia(sf.Space).WithTrailingTrivia(sf.Space))
                .WithSemicolonToken(sf.Token(sk.SemicolonToken))
                .WithLeadingTrivia(keywordLeading)
        assignStatement

module NavCode =
    
    
    
       
    let hd l  = l |> List.head   
    
//    let rec buildUnaryExpression (level:int) (op,a1) : CodeExpressionSyntax =
//        match op with
//        | Not ->
//            let innerExp = a1 |> transformNavExpression level |> hd
//            let cdinner = innerExp :?> CodeExpressionSyntax
//            let notexp =
//                sf.UnaryExpression(SyntaxKind.UnaryNotExpression,innerExp)
//                    .WithOperatorToken(sf.Token(sk.NotKeyword).WithTrailingTrivia(sf.Space))
//            notexp
//        | Grouping ->
//            let innerExp = a1 |> buildExpression :?> CodeExpressionSyntax
//            sf.ParenthesizedExpression(innerExp)
//        | x -> failwithf $"%A{x}"
        
    let buildBinaryExpression (level:int) (op,a1:ALExpression,a2:ALExpression) : CodeExpressionSyntax =
        
            
        let exp1 = a1 |> ALGenExpr.toCodeExpression level
        let exp2 = a2 |> ALGenExpr.toCodeExpression level
        match op with
        | Scope -> sf.OptionAccessExpression(exp1,exp2 :?> IdentifierNameSyntax)
        | MemberAccess -> sf.MemberAccessExpression(exp1,exp2 :?> IdentifierNameSyntax)
        | _ ->
        match op with
        | Add -> sf.BinaryExpression(sk.AddExpression,exp1,exp2)
        | Subtract -> sf.BinaryExpression(sk.SubtractExpression,exp1,exp2)
        | Multiply -> sf.BinaryExpression(sk.MultiplyExpression,exp1,exp2)
        | Divide -> sf.BinaryExpression(sk.DivideExpression,exp1,exp2)
        | Mod -> sf.BinaryExpression(sk.ModuloExpression,exp1,exp2)
        | Equals -> sf.BinaryExpression(sk.EqualsExpression,exp1,exp2)
        | NotEquals -> sf.BinaryExpression(sk.NotEqualsExpression,exp1,exp2)
        | GreaterThan -> sf.BinaryExpression(sk.GreaterThanExpression,exp1,exp2)
        | GreaterThanOrEqual -> sf.BinaryExpression(sk.GreaterThanOrEqualExpression,exp1,exp2)
        | LessThan -> sf.BinaryExpression(sk.LessThanExpression,exp1,exp2)
        | LessThanOrEqual -> sf.BinaryExpression(sk.LessThanOrEqualExpression,exp1,exp2)
        | OR -> sf.BinaryExpression(sk.LogicalOrExpression,exp1,exp2)
        | AND -> sf.BinaryExpression(sk.LogicalAndExpression,exp1,exp2)
        | XOR -> sf.BinaryExpression(sk.LogicalXorExpression,exp1,exp2)
        | Range -> sf.BinaryExpression(sk.RangeExpression,exp1,exp2)
        | x -> failwithf $"%A{x}"
        |> (fun f -> f.WithOperatorToken(f.OperatorToken.WithLeadingTrivia(sf.Space).WithTrailingTrivia(sf.Space)))

    
    let rec buildNaryExpression (level:int) (exp:ALNaryExpression) =
        let nar = 5
        match exp with
        | Invocation(alExpression, alExpressions) ->
            let tgt = alExpression |> ALGenExpr.toCodeExpression level
            let args =
                alExpressions
                |> List.map (fun f -> ALGenExpr.toCodeExpression level f)
            let separatedSyntaxList = SeparatedSyntaxList()
            match args with
            | [] -> sf.InvocationExpression(tgt)
            | _ -> 
                let newl = separatedSyntaxList.AddRange(args)
                let nal = sf.ArgumentList(newl)
                sf.InvocationExpression(tgt,nal)
            
        | x -> failwithf $"%A{x}"
    let rec transformExpression (level:int) (head:ALExpression) : NavExpr list =
        let leveltrivia = [| for i = 0 to level do Trivia._4spc |] |> Array.concat
   
        match head with
        | NaryExpression naryExp ->
            let alexp = naryExp |> buildNaryExpression level
            alexp :> ExpressionSyntax |> Exp |> List.singleton
        | Binary (op,a1,a2) ->
            (op,a1,a2) |> buildBinaryExpression level :> ExpressionSyntax |> Exp |> List.singleton
        | Identifier s ->
            sf.IdentifierName(s.Name) :> ExpressionSyntax |> Exp |> List.singleton
        | Constant o ->
            match o with
            | :? int as v -> 
                sf.Literal(v)
                |> sf.Int32SignedLiteralValue
                |> sf.LiteralExpression
                :> ExpressionSyntax
                |> ALGenExpr.ofSingle
            | :? bool as v ->
                let lit =
                    if v then sf.BooleanLiteralValue(sf.Token(sk.TrueKeyword))
                    else sf.BooleanLiteralValue(sf.Token(sk.FalseKeyword))
                lit
                |> sf.LiteralExpression
                :> ExpressionSyntax
                |> ALGenExpr.ofSingle
            | :? string as v ->
                sf.Literal(v)
                |> sf.StringLiteralValue
                |> sf.LiteralExpression
                :> ExpressionSyntax
                |> ALGenExpr.ofSingle
            | :? double as v ->
                let token =  v.ToString($"{0:D}",CultureInfo.InvariantCulture) |> sf.ParseToken
                match token.Kind with
                | SyntaxKind.DecimalLiteralToken ->
                    sf.DecimalSignedLiteralValue(token)
                    |> sf.LiteralExpression
                    :> ExpressionSyntax
                    |> ALGenExpr.ofSingle
                | SyntaxKind.Int32LiteralToken ->
                    token
                    |> sf.Int32SignedLiteralValue
                    |> sf.LiteralExpression
                    :> ExpressionSyntax
                    |> ALGenExpr.ofSingle
                | _ -> failwith "unknown token"
            | _ ->
                sf.Literal(o :?> string)
                |> sf.StringLiteralValue
                |> sf.LiteralExpression
                :> ExpressionSyntax
                |> ALGenExpr.ofSingle
        | UnaryExp (op,a1) ->
            failwith "unary expr not implemented"
//            let alExp = (op,a1) |> buildUnaryExpression
//            alExp
            
            
        | Assignment(alExpression, expression) ->
            let t = 5
            let statement =
                ALGenExpr.assignment level (alExpression,expression)
                :> StatementSyntax
                |> Stat
            [statement]
        | IfStatement(guard, thenExp, elseExpOpt) ->
            failwith "if statements not implemented"
//            let statement = GenALStatement.createIfElse level guard thenExp elseExpOpt            
//            transformExpression level (statement::acc) tail
        | Exit alExpression ->
            let leveltrivia = [| for i = 0 to level do Trivia._4spc |] |> Array.concat
            let leadingTrivia = Array.append ALGen.Trivia.lf4spc leveltrivia
            let alexp = alExpression |> transformExpression level |> ALGenExpr.toSingleCodeExpression
            let exitstatement =
                sf.ExitStatement(alexp)
                    .WithLeadingTrivia(leadingTrivia)
                    .WithOpenParenthesisToken(sf.Token(sk.OpenParenToken))
                    .WithCloseParenthesisToken(sf.Token(sk.CloseParenToken))
                    .WithSemicolonToken(sf.Token(sk.SemicolonToken))
            [exitstatement |> ALGenExpr.castStat]
        | ForLoop(loopvariable,initval, endval, doStatement, isUp) ->
            failwith "for loops not implemented"
//            let loopvar = sf.IdentifierName("i")
//            let loopvar = loopvariable |> transformExpression level |> Expression.toSingleCodeExpression
//            let doStatementExpanded =
//                match doStatement with
//                | Assignment(alExpression, expression) -> 
//                    GenALStatement.createAssignment level (alExpression,expression) :> StatementSyntax
//                | Block alStatements ->
//                    let statements = transformExpression (level+1) [] alStatements
//                    let v = GenALStatement.createBlock level statements
//                    v
//                
//                
//                | _ -> raise (NotImplementedException())
            
//            let l2_init  =
//                match initval with
//                | Constant o ->
//                    let intv : int = unbox o
//                    sf.Literal(intv)
//                    |> sf.Int32SignedLiteralValue
//                    |> sf.LiteralExpression
//                | _ -> failwith ""
//            let l4_end = GenALExpression.buildExpression endval :?> CodeExpressionSyntax
//            let statement =        
//                sf.ForStatement(
//                    loopvar.WithLeadingTrivia(sf.Space).WithTrailingTrivia(sf.Space),// |> (t.wi >> t.wts),
//                    l2_init.WithLeadingTrivia(sf.Space).WithTrailingTrivia(sf.Space),
//                    sf.Token(if isUp then sk.ToKeyword else sk.DownToKeyword),
//                    l4_end.WithLeadingTrivia(sf.Space).WithTrailingTrivia(sf.Space),
//                    doStatementExpanded.WithLeadingTrivia(sf.Space).WithTrailingTrivia(sf.Space))
//            transformExpression level (statement::acc) tail
        | WhileLoop(fsal_guard, fsal_do) ->
            failwith "while loops not implemented"
//            let al_guard = fsal_guard |> GenALExpression.buildExpression :?> CodeExpressionSyntax
//            let al_do =
//                fsal_do |> ALStatement.ofExpression
//                |> (fun f -> transformExpression level [] [f] )
//                |> List.head
//            let statement =
//                sf.WhileStatement(al_guard, al_do)
//                    .WithDoKeywordToken(sf.Token(sk.DoKeyword) |> (t.wlst >> t.wtst) )
//                    .WithWhileKeywordToken(sf.Token(sk.WhileKeyword) |> t.wtst)
//            transformExpression level (statement::acc) tail
        | Block exprs ->
            let statements' =
                exprs
                |> List.collect (transformExpression level)
                |> ALGenExpr.chooseStat
                |> List.map (fun f ->
                    let currtrivia = f.GetLeadingTrivia()
                    f.WithLeadingTrivia(currtrivia.AddRange(Trivia._4spc))
                )
            
            let sl =
                if level = 0
                then sf.List(statements') // TODO: listrev
                else
                    let test = 5
                    sf.List(statements')
            let statement =
                sf.Block(sl)
                    .WithBeginKeywordToken(sf.Token(sk.BeginKeyword))
                    .WithEndKeywordToken(sf.Token(sk.EndKeyword).WithLeadingTrivia(Trivia.lf8spc))
                    .WithSemicolonToken(sf.Token(sk.SemicolonToken))
            statement |> ALGenExpr.castStat |> List.singleton
//        | ForeachLoop(identif, container, doStatement) ->
//            let statement = GenALStatement.foreachStatement level identif container doStatement
//            transformExpression level (statement::acc) tail
        | x -> failwithf $"%A{x}"
