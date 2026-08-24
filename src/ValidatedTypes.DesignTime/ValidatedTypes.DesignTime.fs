module ValidatedTypesImplementation

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
open FSharp.Core.CompilerServices
open MyNamespace
open ProviderImplementation
open ProviderImplementation.ProvidedTypes
open ValidatedTypes.DesignTime
// open ValidatedTypes.DesignTime.Number


// Put any utility helpers here
[<AutoOpen>]
module internal Helpers =

        // Extracts the MethodInfo behind a two-argument top-level function, e.g.
        //   methodInfoOf2 <@ fun (a: float) (b: string) -> Number.create a b @>
        let methodInfoOf2 (expr: Expr<'a -> 'b -> 'c>) =
            match expr with
            | Lambdas(_, Call(_, mi, _)) -> mi
            | _ -> failwith "Expected a quotation of the form <@ fun a b -> SomeModule.someFunction a b @>"

        let createType<'T, 'V> (parser: string -> Result<'V, list<string>>) (creator: MethodInfo) (constrains: string) typeName thisAssembly namespaceName   =

            match parser constrains with
            | Error errors ->
                // Qualified as System.String: ValidatedTypes.DesignTime.String (our own module,
                // opened above) shadows the BCL/FSharp.Core String module here.
                let errorMessages : string = System.String.Join("; ", errors)
                raise (ArgumentException errorMessages)

            | Ok _ ->
                // The Ok case above is only a design-time validation gate - it
                // catches malformed constraint JSON as a compile-time error.
                let t = ProvidedTypeDefinition(thisAssembly, namespaceName, typeName, Some typeof<obj>)

                let tryCreate =
                    ProvidedMethod(
                        methodName = "create",
                        parameters = [ ProvidedParameter("value", typeof<'T>) ],
                        returnType = typeof<Result<'T, list<string>>>,
                        isStatic = true,
                        invokeCode = (fun args ->
                            Expr.Call(creator, [ args.[0]; Expr.Value(constrains) ])
                        )
                    )

                t.AddMember tryCreate


                t

[<TypeProvider>]
type BasicErasingProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config, addDefaultProbingLocation=true)

    let namespaceName  = "ValidatedTypes.Provided"
    let thisAssembly  = Assembly.GetExecutingAssembly()

    // Create Number
    let numberCreateMethod =
        methodInfoOf2 <@ fun (value: float) (validationsJson: string) -> Number.create value validationsJson @>

    let createNumber : string -> string -> Assembly -> string -> ProvidedTypeDefinition =
        createType<float, _> Number.parse numberCreateMethod

    // Create Integer
    let integerCreateMethod =
        methodInfoOf2 <@ fun (value: int) (validationsJson: string) -> Integer.create value validationsJson @>

    let createInteger : string -> string -> Assembly -> string -> ProvidedTypeDefinition =
        createType<int, _> Integer.parse integerCreateMethod

    // Create String
    let stringCreateMethod =
        methodInfoOf2 <@ fun (value: string) (validationsJson: string) -> String.create value validationsJson @>

    let createString : string -> string -> Assembly -> string -> ProvidedTypeDefinition =
        createType<string, _> String.parse stringCreateMethod

    // Create List
    // Items are erased to obj: the validations here (minItems/maxItems/uniqueItems) are
    // purely structural and never inspect item content, so there is no concrete element
    // type to fix statically the way Number/Integer/String fix their single value type.
    let listCreateMethod =
        methodInfoOf2 <@ fun (value: obj list) (validationsJson: string) -> List.create value validationsJson @>

    let createList : string -> string -> Assembly -> string -> ProvidedTypeDefinition =
        createType<obj list, _> List.parse listCreateMethod


    do
        // Define the provided type "Number"
        let NumberType = ProvidedTypeDefinition(thisAssembly, namespaceName, "Number", Some typeof<obj>)

        // Define the static parameters for the "Number" type
        // "Json" is intentionally required (no default value): if it were optional, an
        // explicit argument that happens to equal the default (e.g. "{}") would collide
        // with the ProvidedTypes SDK's "all static params match their defaults" case,
        // which skips DefineStaticParameters' callback and returns the plain, member-less
        // base type instead - see the SDK's own warning about all-optional static params.
        NumberType.DefineStaticParameters(
            [
                ProvidedStaticParameter("Json", typeof<string>)
            ],
            fun typeName args -> createNumber (args.[0] :?> string) typeName thisAssembly namespaceName)

        this.AddNamespace(namespaceName, [NumberType])

        // Define the provided type "Integer"
        let IntegerType = ProvidedTypeDefinition(thisAssembly, namespaceName, "Integer", Some typeof<obj>)

        IntegerType.DefineStaticParameters(
            [
                ProvidedStaticParameter("Json", typeof<string>)
            ],
            fun typeName args -> createInteger (args.[0] :?> string) typeName thisAssembly namespaceName)

        this.AddNamespace(namespaceName, [IntegerType])

        // Define the provided type "String"
        let StringType = ProvidedTypeDefinition(thisAssembly, namespaceName, "String", Some typeof<obj>)

        StringType.DefineStaticParameters(
            [
                ProvidedStaticParameter("Json", typeof<string>)
            ],
            fun typeName args -> createString (args.[0] :?> string) typeName thisAssembly namespaceName)

        this.AddNamespace(namespaceName, [StringType])

        // Define the provided type "List"
        let ListType = ProvidedTypeDefinition(thisAssembly, namespaceName, "List", Some typeof<obj>)

        ListType.DefineStaticParameters(
            [
                ProvidedStaticParameter("Json", typeof<string>)
            ],
            fun typeName args -> createList (args.[0] :?> string) typeName thisAssembly namespaceName)

        this.AddNamespace(namespaceName, [ListType])

