namespace ValidatedTypes.DesignTime
open JsonSchemaProvider
open System
open FsToolkit.ErrorHandling

module Integer =

    // Use a JSON schema to define the structure of the input for integer validations
    // #region validation-input-schema
    [<Literal>]
    let integerValidationInputSchema =
        """
        {
            "type": "object",
            "properties": {
                "minimum": { "type": "number" },
                "maximum": { "type": "number" },
                "exclusiveMinimum": { "type": "number" },
                "exclusiveMaximum": { "type": "number" },
                "multipleOf": { "type": "number" }
            }
        }
        """

    // #omit-start

    // Define a type to represent the validations for a integer
    // #omit-end
    type IntegerValidations =
        JsonSchemaProvider<schema=integerValidationInputSchema>
    // #endregion

    // Validate minimum
    let minimum (validations: IntegerValidations) (integer: int)  =
        match validations.minimum with
        | None -> Ok integer
        | Some min when float integer >= min -> Ok integer
        | Some min -> Error (sprintf "Value %i is less than the minimum allowed value %g" integer min)

    // Validate maximum
    let maximum (validations: IntegerValidations) (integer: int)  =
        match validations.maximum with
        | None -> Ok integer
        | Some max when max >= float integer -> Ok integer
        | Some max -> Error <| sprintf "Value %i is greater than the maximum allowed value %g" integer max

    let exclusiveMinimum (validations: IntegerValidations) (integer: int)  =
        match validations.exclusiveMinimum with
        | None -> Ok integer
        | Some min when float integer > min -> Ok integer
        | Some min -> Error <| sprintf "Value %i is equal too or less than the minimum allowed value %g" integer min

    // Validate maximum
    let exclusiveMaximum (validations: IntegerValidations) (integer: int)  =
        match validations.exclusiveMaximum with
        | None -> Ok integer
        | Some max when max > float integer -> Ok integer
        | Some max -> Error <| sprintf "Value %i is equal too or greater than the maximum allowed value %g" integer max

    let multipleOf (validations: IntegerValidations) (integer: int)  =
        match validations.multipleOf with
        | None -> Ok integer
        | Some multiple when float integer % multiple = 0.0 -> Ok integer
        | Some multiple -> Error <| sprintf "Value %i is not a multiple of %g" integer multiple

    // Parse input JSON string into IntegerValidations
    let parse (validationsJson: string) : Result<IntegerValidations, list<string>> =
        try
            Ok <| IntegerValidations.Parse validationsJson
        with
        | ex -> Error <| [sprintf "Invalid JSON schema for integer validations. Hint (The json is validated against following schema): %s" integerValidationInputSchema; ex.Message]

    // Run the validations on a integer.
    let create (integer: int) (validationsJson: string) : Result<int, list<string>> =
        result {
            let! validations = parse validationsJson

            // Collect all validation errors
            let errors =
                [minimum; maximum; exclusiveMinimum; exclusiveMaximum; multipleOf]
                |> List.choose (fun validation ->
                    match validation validations integer with
                    | Ok _ -> None
                    | Error err -> Some err)

            return!
                if List.isEmpty errors then
                    Ok integer
                else
                    Error errors
        }





