module StringTests

open ValidatedTypes.Provided
open NUnit.Framework

// ---- minLength ----

type MinLength3 = String<"""{"minLength": 3}""">

[<Test>]
let ``minLength: value longer than minimum succeeds`` () =
    match MinLength3.create("hello") with
    | Ok v -> Assert.That(v, Is.EqualTo("hello"))
    | Error errs -> Assert.Fail(String.concat "; " errs)

[<Test>]
let ``minLength: value equal to minimum succeeds`` () =
    match MinLength3.create("abc") with
    | Ok v -> Assert.That(v, Is.EqualTo("abc"))
    | Error errs -> Assert.Fail(String.concat "; " errs)

[<Test>]
let ``minLength: value shorter than minimum fails`` () =
    match MinLength3.create("ab") with
    | Error _ -> Assert.Pass()
    | Ok _ -> Assert.Fail("expected a validation error")

// ---- maxLength ----

type MaxLength5 = String<"{\"maxLength\": 5}">

[<Test>]
let ``maxLength: value shorter than maximum succeeds`` () =
    match MaxLength5.create("abc") with
    | Ok v -> Assert.That(v, Is.EqualTo("abc"))
    | Error errs -> Assert.Fail(String.concat "; " errs)

[<Test>]
let ``maxLength: value equal to maximum succeeds`` () =
    match MaxLength5.create("abcde") with
    | Ok v -> Assert.That(v, Is.EqualTo("abcde"))
    | Error errs -> Assert.Fail(String.concat "; " errs)

[<Test>]
let ``maxLength: value longer than maximum fails`` () =
    match MaxLength5.create("abcdef") with
    | Error _ -> Assert.Pass()
    | Ok _ -> Assert.Fail("expected a validation error")

// ---- pattern ----

type DigitsOnly = String<"{\"pattern\": \"^[0-9]+$\"}">

[<Test>]
let ``pattern: matching value succeeds`` () =
    match DigitsOnly.create("12345") with
    | Ok v -> Assert.That(v, Is.EqualTo("12345"))
    | Error errs -> Assert.Fail(String.concat "; " errs)

[<Test>]
let ``pattern: non-matching value fails`` () =
    match DigitsOnly.create("12a45") with
    | Error _ -> Assert.Pass()
    | Ok _ -> Assert.Fail("expected a validation error")

// ---- format: email ----

type Email = String<"{\"format\": \"email\"}">

[<Test>]
let ``format email: valid address succeeds`` () =
    match Email.create("user@example.com") with
    | Ok v -> Assert.That(v, Is.EqualTo("user@example.com"))
    | Error errs -> Assert.Fail(String.concat "; " errs)

[<Test>]
let ``format email: invalid address fails`` () =
    match Email.create("not-an-email") with
    | Error _ -> Assert.Pass()
    | Ok _ -> Assert.Fail("expected a validation error")

// ---- format: date ----

type Date = String<"{\"format\": \"date\"}">

[<Test>]
let ``format date: valid date succeeds`` () =
    match Date.create("2024-01-31") with
    | Ok v -> Assert.That(v, Is.EqualTo("2024-01-31"))
    | Error errs -> Assert.Fail(String.concat "; " errs)

[<Test>]
let ``format date: invalid date fails`` () =
    match Date.create("31-01-2024") with
    | Error _ -> Assert.Pass()
    | Ok _ -> Assert.Fail("expected a validation error")

// ---- format: unknown format is annotation-only ----

type UnknownFormat = String<"{\"format\": \"totally-made-up\"}">

[<Test>]
let ``format unknown: any value succeeds`` () =
    match UnknownFormat.create("whatever") with
    | Ok v -> Assert.That(v, Is.EqualTo("whatever"))
    | Error errs -> Assert.Fail(String.concat "; " errs)

// ---- combined constraints ----

type Username = String<"{\"minLength\": 3, \"maxLength\": 12, \"pattern\": \"^[a-zA-Z0-9_]+$\"}">

[<Test>]
let ``combined: value satisfying all constraints succeeds`` () =
    match Username.create("valid_user1") with
    | Ok v -> Assert.That(v, Is.EqualTo("valid_user1"))
    | Error errs -> Assert.Fail(String.concat "; " errs)

[<Test>]
let ``combined: value violating pattern fails`` () =
    match Username.create("no spaces!") with
    | Error _ -> Assert.Pass()
    | Ok _ -> Assert.Fail("expected a validation error")

[<Test>]
let ``combined: value too short fails`` () =
    match Username.create("ab") with
    | Error _ -> Assert.Pass()
    | Ok _ -> Assert.Fail("expected a validation error")

// ---- no constraints ----

type NoConstraints = String<"{}">

[<Test>]
let ``no constraints: any value succeeds`` () =
    match NoConstraints.create("anything at all") with
    | Ok v -> Assert.That(v, Is.EqualTo("anything at all"))
    | Error errs -> Assert.Fail(String.concat "; " errs)
