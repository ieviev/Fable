[<RequireQualifiedAccess>]
module Fable.Core.AL

open System
//
//[<Import("typing", "Callable")>]
//type [<AllowNullLiteral>] Callable =
//    [<Emit "$0.__name__)">] abstract name: string
//    [<Emit "$0$1...">] abstract Invoke: [<ParamArray>] args: obj[] -> obj
//    [<Emit "$0">] abstract Instance: obj
  
[<AttributeUsage(AttributeTargets.All,Inherited=true,AllowMultiple=false)>]
type Name(str:string) = 
    inherit System.Attribute()
    member this.Name : string = str

/// single-instance codeunit for modules
[<AttributeUsage(AttributeTargets.Struct|||AttributeTargets.Class,Inherited=true,AllowMultiple=false)>]
type Codeunit(id:int) =
    inherit System.Attribute()
    member this.ObjectId : int = id
    
/// table for f# record types
[<AttributeUsage(AttributeTargets.Struct|||AttributeTargets.Class,Inherited=true,AllowMultiple=false)>]     
type Table(id:int) =
    inherit System.Attribute()
    member this.ObjectId : int = id

/// compile to JsonToken accesses in AL
[<AttributeUsage(AttributeTargets.Struct|||AttributeTargets.Class,Inherited=true,AllowMultiple=false)>]
type Json() = 
    inherit System.Attribute()
    
/// compile to Option values in AL
[<AttributeUsage(AttributeTargets.Struct|||AttributeTargets.Class,Inherited=true,AllowMultiple=false)>]
type Option() = 
    inherit System.Attribute()


[<AbstractClass>]
type ALSimpleType() = do ()

[<AbstractClass>]
type ALComplexType() = do ()
    
[<AbstractClass>]    
type ALObjectType() =
    inherit ALComplexType()
    abstract member ObjectId : int      

//
//[<AbstractClass>]
//type DecoratorAttribute() =
//    inherit Attribute()
//    abstract Decorate: fn: Callable -> Callable
//
//[<AbstractClass>]
//type ReflectedDecoratorAttribute() =
//    inherit Attribute()
//    abstract Decorate: fn: Callable * info: Reflection.MethodInfo -> Callable
//
//// Hack because currently Fable doesn't keep information about spread for anonymous functions
//[<Emit("lambda *args: $0(args)")>]
//let argsFunc (fn: obj[] -> obj): Callable = nativeOnly
//
//type [<AllowNullLiteral>] ArrayConstructor =
//    [<Emit "$0([None]*$1...)">]
//    abstract Create: size: int -> 'T[]
//    [<Emit "isinstance($1, list)">]
//    abstract isArray: arg: obj -> bool
//    abstract from: arg: obj -> 'T[]
//
//and [<AllowNullLiteral>] ArrayBuffer =
//    abstract byteLength: int
//    [<Emit("$0[$1:$1+$2]")>]
//    abstract slice: ``begin``: int * ?``end``: int -> ArrayBuffer
//
//[<RequireQualifiedAccess>]
//module Constructors =
//    let [<Emit("list")>] Array: ArrayConstructor = nativeOnly
