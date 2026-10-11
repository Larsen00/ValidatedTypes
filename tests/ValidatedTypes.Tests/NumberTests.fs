module NumberTests

open ValidatedTypes.Provided
open NUnit.Framework

// ---- minimum ----

type Minimum1 = Number<"""{"minimum": 1}""">

[<Test>]
let ``minimum: value above minimum succeeds`` () =
    match Minimum1.create(5.0) with
    | Ok v -> Assert.That(v, Is.EqualTo(5.0))
    | Error errs -> Assert.Fail(String.concat "; " errs)

[<Test>]
let ``minimum: value equal to minimum succeeds`` () =
    match Minimum1.create(1.0) with
    | Ok v -> Assert.That(v, Is.EqualTo(1.0))
    | Error errs -> Assert.Fail(String.concat "; " errs)

[<Test>]
let ``minimum: value below minimum fails`` () =
    match Minimum1.create(0.0) with
    | Error _ -> Assert.Pass()
    | Ok _ -> Assert.Fail("expected a validation error")

// ---- maximum ----

type Maximum10 = Number<"""{"maximum": 10}""">

[<Test>]
let ``maximum: value below maximum succeeds`` () =
    match Maximum10.create(5.0) with
    | Ok v -> Assert.That(v, Is.EqualTo(5.0))
    | Error errs -> Assert.Fail(String.concat "; " errs)

[<Test>]
let ``maximum: value equal to maximum succeeds`` () =
    match Maximum10.create(10.0) with
    | Ok v -> Assert.That(v, Is.EqualTo(10.0))
    | Error errs -> Assert.Fail(String.concat "; " errs)

[<Test>]
let ``maximum: value above maximum fails`` () =
    match Maximum10.create(11.0) with
    | Error _ -> Assert.Pass()
    | Ok _ -> Assert.Fail("expected a validation error")

// ---- exclusiveMinimum ----

type ExclusiveMinimum1 = Number<"""{"exclusiveMinimum": 1}""">

[<Test>]
let ``exclusiveMinimum: value above minimum succeeds`` () =
    match ExclusiveMinimum1.create(2.0) with
    | Ok v -> Assert.That(v, Is.EqualTo(2.0))
    | Error errs -> Assert.Fail(String.concat "; " errs)

[<Test>]
let ``exclusiveMinimum: value equal to minimum fails`` () =
    match ExclusiveMinimum1.create(1.0) with
    | Error _ -> Assert.Pass()
    | Ok _ -> Assert.Fail("expected a validation error")

[<Test>]
let ``exclusiveMinimum: value below minimum fails`` () =
    match ExclusiveMinimum1.create(0.0) with
    | Error _ -> Assert.Pass()
    | Ok _ -> Assert.Fail("expected a validation error")

// ---- exclusiveMaximum ----

type ExclusiveMaximum10 = Number<"""{"exclusiveMaximum": 10}""">

[<Test>]
let ``exclusiveMaximum: value below maximum succeeds`` () =
    match ExclusiveMaximum10.create(5.0) with
    | Ok v -> Assert.That(v, Is.EqualTo(5.0))
    | Error errs -> Assert.Fail(String.concat "; " errs)

[<Test>]
let ``exclusiveMaximum: value equal to maximum fails`` () =
    match ExclusiveMaximum10.create(10.0) with
    | Error _ -> Assert.Pass()
    | Ok _ -> Assert.Fail("expected a validation error")

[<Test>]
let ``exclusiveMaximum: value above maximum fails`` () =
    match ExclusiveMaximum10.create(11.0) with
    | Error _ -> Assert.Pass()
    | Ok _ -> Assert.Fail("expected a validation error")

// ---- multipleOf ----

type MultipleOf3 = Number<"""{"multipleOf": 3}""">

[<Test>]
let ``multipleOf: exact multiple succeeds`` () =
    match MultipleOf3.create(9.0) with
    | Ok v -> Assert.That(v, Is.EqualTo(9.0))
    | Error errs -> Assert.Fail(String.concat "; " errs)

[<Test>]
let ``multipleOf: zero succeeds`` () =
    match MultipleOf3.create(0.0) with
    | Ok v -> Assert.That(v, Is.EqualTo(0.0))
    | Error errs -> Assert.Fail(String.concat "; " errs)

[<Test>]
let ``multipleOf: non-multiple fails`` () =
    match MultipleOf3.create(10.0) with
    | Error _ -> Assert.Pass()
    | Ok _ -> Assert.Fail("expected a validation error")

// ---- combined constraints ----

// #region validatedtypes-usage
type Range1To10 = Number<"""{"minimum": 1, "maximum": 10}""">

let message =
    match Range1To10.create(5.0) with
    | Ok value -> sprintf "Valid: %g" value
    | Error errors -> sprintf "Invalid: %A" errors
// #endregion

[<Test>]
let ``usage example: valid value gives Valid message`` () =
    Assert.That(message, Is.EqualTo("Valid: 5"))

[<Test>]
let ``range: value inside bounds succeeds`` () =
    match Range1To10.create(5.0) with
    | Ok v -> Assert.That(v, Is.EqualTo(5.0))
    | Error errs -> Assert.Fail(String.concat "; " errs)

[<Test>]
let ``range: value below lower bound fails`` () =
    match Range1To10.create(0.0) with
    | Error _ -> Assert.Pass()
    | Ok _ -> Assert.Fail("expected a validation error")

[<Test>]
let ``range: value above upper bound fails`` () =
    match Range1To10.create(11.0) with
    | Error _ -> Assert.Pass()
    | Ok _ -> Assert.Fail("expected a validation error")

// ---- no constraints ----

type NoConstraints = Number<"""{}""">

[<Test>]
let ``no constraints: any value succeeds`` () =
    match NoConstraints.create(-1234.5) with
    | Ok v -> Assert.That(v, Is.EqualTo(-1234.5))
    | Error errs -> Assert.Fail(String.concat "; " errs)
