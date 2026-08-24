
namespace ValidatedTypes.DesignTime
open JsonSchemaProvider
open System
open FsToolkit.ErrorHandling

module List =

    // Use a JSON schema to define the structure of the input for list validations
    [<Literal>]
    let listValidationInputSchema =
        """
        {
            "type": "object",
            "properties": {
                "minItems" : { "type": "integer" },
                "maxItems" : { "type": "integer" },
                "uniqueItems" : { "type": "boolean" }
            }
        }
        """

    // Define a type to represent the validations for a list
    type ListValidations = JsonSchemaProvider<schema=listValidationInputSchema>

    // Validate minItems
    let minItems (validations: ListValidations) (list: list<'T>) =
        match validations.minItems with
        | None -> Ok list
        | Some min when List.length list >= min -> Ok list
        | Some min -> Error (sprintf "List has %d items, which is less than the minimum allowed %d" (List.length list) min)

    // Validate maxItems
    let maxItems (validations: ListValidations) (list: list<'T>) =
        match validations.maxItems with
        | None -> Ok list
        | Some max when List.length list <= max -> Ok list
        | Some max -> Error (sprintf "List has %d items, which is greater than the maximum allowed %d" (List.length list) max)

    // Validate uniqueItems
    // Uses equality rather than comparison: erased list items arrive as plain 'obj',
    // which never satisfies F#'s 'comparison' constraint (needed by Set/sort), but does
    // satisfy 'equality' - and boxed values still dispatch to their real Equals override.
    let uniqueItems (validations: ListValidations) (list: list<'T>) =
        match validations.uniqueItems with
        | Some false
        | None -> Ok list
        | Some true when List.length list = (List.distinct list |> List.length) -> Ok list
        | Some true -> Error "List contains duplicate items, but uniqueItems is set to true"

    // Parse input JSON string into ListValidations
    let parse (validationsJson: string) : Result<ListValidations, list<string>> =
        try
            Ok <| ListValidations.Parse validationsJson
        with
        | ex -> Error <| [sprintf "Invalid JSON schema for list validations. Hint (The json is validated against following schema): %s" listValidationInputSchema; ex.Message]

    // Run the validations on a list.
    let create (list: list<'T>) (validationsJson: string) : Result<list<'T>, list<string>> =
        result {
            let! validations = parse validationsJson

            // Collect all validation errors
            let errors =
                [minItems; maxItems; uniqueItems]
                |> List.choose (fun validation ->
                    match validation validations list with
                    | Ok _ -> None
                    | Error err -> Some err)

            return!
                if List.isEmpty errors then
                    Ok list
                else
                    Error errors
        }





