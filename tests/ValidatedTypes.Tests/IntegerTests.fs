module IntegerTests

open ValidatedTypes.Provided
open NUnit.Framework

// ---- minimum ----

type IntMinimum1 = Integer<"{\"minimum\": 1}">

[<Test>]
let ``integer minimum: value above minimum succeeds`` () =
    match IntMinimum1.create(5) with
    | Ok v -> Assert.That(v, Is.EqualTo(5))
    | Error errs -> Assert.Fail(String.concat "; " errs)

[<Test>]
let ``integer minimum: value equal to minimum succeeds`` () =
    match IntMinimum1.create(1) with
    | Ok v -> Assert.That(v, Is.EqualTo(1))
    | Error errs -> Assert.Fail(String.concat "; " errs)

[<Test>]
let ``integer minimum: value below minimum fails`` () =
    match IntMinimum1.create(0) with
    | Error _ -> Assert.Pass()
    | Ok _ -> Assert.Fail("expected a validation error")

// ---- maximum ----

type IntMaximum10 = Integer<"{\"maximum\": 10}">

[<Test>]
let ``integer maximum: value below maximum succeeds`` () =
    match IntMaximum10.create(5) with
    | Ok v -> Assert.That(v, Is.EqualTo(5))
    | Error errs -> Assert.Fail(String.concat "; " errs)

[<Test>]
let ``integer maximum: value equal to maximum succeeds`` () =
    match IntMaximum10.create(10) with
    | Ok v -> Assert.That(v, Is.EqualTo(10))
    | Error errs -> Assert.Fail(String.concat "; " errs)

[<Test>]
let ``integer maximum: value above maximum fails`` () =
    match IntMaximum10.create(11) with
    | Error _ -> Assert.Pass()
    | Ok _ -> Assert.Fail("expected a validation error")

// ---- exclusiveMinimum ----

type IntExclusiveMinimum1 = Integer<"{\"exclusiveMinimum\": 1}">

[<Test>]
let ``integer exclusiveMinimum: value above minimum succeeds`` () =
    match IntExclusiveMinimum1.create(2) with
    | Ok v -> Assert.That(v, Is.EqualTo(2))
    | Error errs -> Assert.Fail(String.concat "; " errs)

[<Test>]
let ``integer exclusiveMinimum: value equal to minimum fails`` () =
    match IntExclusiveMinimum1.create(1) with
    | Error _ -> Assert.Pass()
    | Ok _ -> Assert.Fail("expected a validation error")

[<Test>]
let ``integer exclusiveMinimum: value below minimum fails`` () =
    match IntExclusiveMinimum1.create(0) with
    | Error _ -> Assert.Pass()
    | Ok _ -> Assert.Fail("expected a validation error")

// ---- exclusiveMaximum ----

type IntExclusiveMaximum10 = Integer<"{\"exclusiveMaximum\": 10}">

[<Test>]
let ``integer exclusiveMaximum: value below maximum succeeds`` () =
    match IntExclusiveMaximum10.create(5) with
    | Ok v -> Assert.That(v, Is.EqualTo(5))
    | Error errs -> Assert.Fail(String.concat "; " errs)

[<Test>]
let ``integer exclusiveMaximum: value equal to maximum fails`` () =
    match IntExclusiveMaximum10.create(10) with
    | Error _ -> Assert.Pass()
    | Ok _ -> Assert.Fail("expected a validation error")

[<Test>]
let ``integer exclusiveMaximum: value above maximum fails`` () =
    match IntExclusiveMaximum10.create(11) with
    | Error _ -> Assert.Pass()
    | Ok _ -> Assert.Fail("expected a validation error")

// ---- multipleOf ----

type IntMultipleOf3 = Integer<"{\"multipleOf\": 3}">

[<Test>]
let ``integer multipleOf: exact multiple succeeds`` () =
    match IntMultipleOf3.create(9) with
    | Ok v -> Assert.That(v, Is.EqualTo(9))
    | Error errs -> Assert.Fail(String.concat "; " errs)

[<Test>]
let ``integer multipleOf: zero succeeds`` () =
    match IntMultipleOf3.create(0) with
    | Ok v -> Assert.That(v, Is.EqualTo(0))
    | Error errs -> Assert.Fail(String.concat "; " errs)

[<Test>]
let ``integer multipleOf: non-multiple fails`` () =
    match IntMultipleOf3.create(10) with
    | Error _ -> Assert.Pass()
    | Ok _ -> Assert.Fail("expected a validation error")

// ---- combined constraints ----

type IntRange1To10 = Integer<"{\"minimum\": 1, \"maximum\": 10}">

[<Test>]
let ``integer range: value inside bounds succeeds`` () =
    match IntRange1To10.create(5) with
    | Ok v -> Assert.That(v, Is.EqualTo(5))
    | Error errs -> Assert.Fail(String.concat "; " errs)

[<Test>]
let ``integer range: value below lower bound fails`` () =
    match IntRange1To10.create(0) with
    | Error _ -> Assert.Pass()
    | Ok _ -> Assert.Fail("expected a validation error")

[<Test>]
let ``integer range: value above upper bound fails`` () =
    match IntRange1To10.create(11) with
    | Error _ -> Assert.Pass()
    | Ok _ -> Assert.Fail("expected a validation error")

// ---- no constraints ----

type IntNoConstraints = Integer<"{}">

[<Test>]
let ``integer no constraints: any value succeeds`` () =
    match IntNoConstraints.create(-1234) with
    | Ok v -> Assert.That(v, Is.EqualTo(-1234))
    | Error errs -> Assert.Fail(String.concat "; " errs)
