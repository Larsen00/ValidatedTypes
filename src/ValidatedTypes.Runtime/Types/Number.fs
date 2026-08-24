namespace ValidatedTypes.DesignTime
open JsonSchemaProvider
open System
open FsToolkit.ErrorHandling

module Number =

    // Use a JSON schema to define the structure of the input for number validations
    [<Literal>]
    let numberValidationInputSchema =
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


    // Define a type to represent the validations for a number
    type NumberValidations = JsonSchemaProvider<schema=numberValidationInputSchema>

    // Validate minimum
    let minimum (validations: NumberValidations) (number: float)  =
        match validations.minimum with
        | None -> Ok number
        | Some min when number >= min -> Ok number
        | Some min -> Error (sprintf "Value %f is less than the minimum allowed value %f" number min)

    // Validate maximum
    let maximum (validations: NumberValidations) (number: float)  =
        match validations.maximum with
        | None -> Ok number
        | Some max when max >= number -> Ok number
        | Some max -> Error <| sprintf "Value %f is greater than the maximum allowed value %f" number max

    let exclusiveMinimum (validations: NumberValidations) (number: float)  =
        match validations.exclusiveMinimum with
        | None -> Ok number
        | Some min when number > min -> Ok number
        | Some min -> Error <| sprintf "Value %f is equal too or less than the minimum allowed value %f" number min

    // Validate maximum
    let exclusiveMaximum (validations: NumberValidations) (number: float)  =
        match validations.exclusiveMaximum with
        | None -> Ok number
        | Some max when max > number -> Ok number
        | Some max -> Error <| sprintf "Value %f is equal too or greater than the maximum allowed value %f" number max

    let multipleOf (validations: NumberValidations) (number: float)  =
        match validations.multipleOf with
        | None -> Ok number
        | Some multiple when number % multiple = 0.0 -> Ok number
        | Some multiple -> Error <| sprintf "Value %f is not a multiple of %f" number multiple

    // Parse input JSON string into NumberValidations
    let parse (validationsJson: string) : Result<NumberValidations, list<string>> =
        try
            Ok <| NumberValidations.Parse validationsJson
        with
        | ex -> Error <| [sprintf "Invalid JSON schema for number validations. Hint (The json is validated against following schema): %s" numberValidationInputSchema; ex.Message]

    // Run the validations on a number.
    let create (number: float) (validationsJson: string) : Result<float, list<string>> =
        result {
            let! validations = parse validationsJson
            
            // Collect all validation errors
            let errors = 
                [minimum; maximum; exclusiveMinimum; exclusiveMaximum; multipleOf]
                |> List.choose (fun validation ->
                    match validation validations number with
                    | Ok _ -> None
                    | Error err -> Some err)
            
            return!
                if List.isEmpty errors then 
                    Ok number 
                else 
                    Error errors
        }




