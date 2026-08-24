
namespace ValidatedTypes.DesignTime
open JsonSchemaProvider
open System
open FsToolkit.ErrorHandling

module String =

    // Use a JSON schema to define the structure of the input for string validations
    [<Literal>]
    let stringValidationInputSchema =
        """
        {
            "type": "object",
            "properties": {
                "minLength": { "type": "integer" },
                "maxLength": { "type": "integer" },
                "pattern": { "type": "string" },
                "format": { "type": "string" }
            }
        }
        """

    // Define a type to represent the validations for a string
    type StringValidations = JsonSchemaProvider<schema=stringValidationInputSchema>


    let minLength (validations: StringValidations) (str: string)  =
        match validations.minLength with
        | None -> Ok str
        | Some min when str.Length >= min -> Ok str
        | Some min -> Error <| sprintf "Value '%s' is shorter than the minimum allowed length %i" str min

    let maxLength (validations: StringValidations) (str: string)  =
        match validations.maxLength with
        | None -> Ok str
        | Some max when str.Length <= max -> Ok str
        | Some max -> Error <| sprintf "Value '%s' is longer than the maximum allowed length %i" str max

    let pattern (validations: StringValidations) (str: string)  =
        match validations.pattern with
        | None -> Ok str
        | Some pattern when System.Text.RegularExpressions.Regex.IsMatch(str, pattern) -> Ok str
        | Some pattern -> Error <| sprintf "Value '%s' does not match the required pattern '%s'" str pattern

    // --- format validators ---

    let private isEmail (s: string) =
        let re = System.Text.RegularExpressions.Regex(@"^[^@\s]+@[^@\s]+\.[^@\s]+$")
        if not (re.IsMatch s) then false
        else
            try System.Net.Mail.MailAddress(s).Address = s
            with _ -> false

    let private isHostname (s: string) =
        System.Text.RegularExpressions.Regex.IsMatch(
            s, @"^(?=.{1,253}$)(?!-)[A-Za-z0-9-]{1,63}(?<!-)(\.(?!-)[A-Za-z0-9-]{1,63}(?<!-))*$")

    let private isIpv4 (s: string) =
        match System.Net.IPAddress.TryParse s with
        | true, ip -> ip.AddressFamily = System.Net.Sockets.AddressFamily.InterNetwork && s.Split('.').Length = 4
        | false, _ -> false

    let private isIpv6 (s: string) =
        match System.Net.IPAddress.TryParse s with
        | true, ip -> ip.AddressFamily = System.Net.Sockets.AddressFamily.InterNetworkV6
        | false, _ -> false

    let private isUuid (s: string) =
        System.Guid.TryParseExact(s, "D") |> fst

    let private isUri (s: string) =
        match System.Uri.TryCreate(s, System.UriKind.Absolute) with
        | true, u -> u.IsAbsoluteUri
        | false, _ -> false

    let private isUriReference (s: string) =
        System.Uri.TryCreate(s, System.UriKind.RelativeOrAbsolute) |> fst

    let private isDateTime (s: string) =
        let shapeOk =
            System.Text.RegularExpressions.Regex.IsMatch(
                s, @"^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(\.\d+)?(Z|[+-]\d{2}:\d{2})$",
                System.Text.RegularExpressions.RegexOptions.IgnoreCase)
        shapeOk
        && (System.DateTimeOffset.TryParseExact(
                s,
                [| "yyyy-MM-ddTHH:mm:ssK"; "yyyy-MM-ddTHH:mm:ss.fffffffK" |],
                System.Globalization.CultureInfo.InvariantCulture,
                System.Globalization.DateTimeStyles.None)
            |> fst)

    let private isDate (s: string) =
        System.DateTime.TryParseExact(
            s, "yyyy-MM-dd", System.Globalization.CultureInfo.InvariantCulture,
            System.Globalization.DateTimeStyles.None)
        |> fst

    let private isRegexPattern (s: string) =
        try System.Text.RegularExpressions.Regex(s) |> ignore; true
        with _ -> false

    // --- dispatch ---

    let format (validations: StringValidations) (str: string) =
        match validations.format with
        | None -> Ok str
        | Some fmt ->
            let isValid =
                match fmt with
                | "email" -> isEmail str
                | "hostname" -> isHostname str
                | "ipv4" -> isIpv4 str
                | "ipv6" -> isIpv6 str
                | "uuid" -> isUuid str
                | "uri" -> isUri str
                | "uri-reference" -> isUriReference str
                | "date-time" -> isDateTime str
                | "date" -> isDate str
                | "regex" -> isRegexPattern str
                | _ -> true // unknown formats are annotations-only per the JSON Schema spec
            if isValid then Ok str
            else Error <| sprintf "Value '%s' does not match the required format '%s'" str fmt




    // Parse input JSON string into StringValidations
    let parse (validationsJson: string) : Result<StringValidations, list<string>> =
        try
            Ok <| StringValidations.Parse validationsJson
        with
        | ex -> Error <| [sprintf "Invalid JSON schema for string validations. Hint (The json is validated against following schema): %s" stringValidationInputSchema; ex.Message]

    // Run the validations on a string.
    let create (str: string) (validationsJson: string) : Result<string, list<string>> =
        result {
            let! validations = parse validationsJson

            // Collect all validation errors
            let errors =
                [minLength; maxLength; pattern; format]
                |> List.choose (fun validation ->
                    match validation validations str with
                    | Ok _ -> None
                    | Error err -> Some err)

            return!
                if List.isEmpty errors then
                    Ok str
                else
                    Error errors
        }





