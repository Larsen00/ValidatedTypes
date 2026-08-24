module ListTests

open ValidatedTypes.Provided
open NUnit.Framework
open NUnit.Framework.Legacy

// ---- minItems ----

type MinItems2 = ValidatedTypes.Provided.List<"{\"minItems\": 2}">

[<Test>]
let ``minItems: length above minimum succeeds`` () =
    match MinItems2.create([ box 1; box 2; box 3 ]) with
    | Ok v -> CollectionAssert.AreEqual([ box 1; box 2; box 3 ], v)
    | Error errs -> Assert.Fail(String.concat "; " errs)

[<Test>]
let ``minItems: length equal to minimum succeeds`` () =
    match MinItems2.create([ box 1; box 2 ]) with
    | Ok v -> CollectionAssert.AreEqual([ box 1; box 2 ], v)
    | Error errs -> Assert.Fail(String.concat "; " errs)

[<Test>]
let ``minItems: length below minimum fails`` () =
    match MinItems2.create([ box 1 ]) with
    | Error _ -> Assert.Pass()
    | Ok _ -> Assert.Fail("expected a validation error")

// ---- maxItems ----

type MaxItems3 = ValidatedTypes.Provided.List<"{\"maxItems\": 3}">

[<Test>]
let ``maxItems: length below maximum succeeds`` () =
    match MaxItems3.create([ box 1; box 2 ]) with
    | Ok v -> CollectionAssert.AreEqual([ box 1; box 2 ], v)
    | Error errs -> Assert.Fail(String.concat "; " errs)

[<Test>]
let ``maxItems: length equal to maximum succeeds`` () =
    match MaxItems3.create([ box 1; box 2; box 3 ]) with
    | Ok v -> CollectionAssert.AreEqual([ box 1; box 2; box 3 ], v)
    | Error errs -> Assert.Fail(String.concat "; " errs)

[<Test>]
let ``maxItems: length above maximum fails`` () =
    match MaxItems3.create([ box 1; box 2; box 3; box 4 ]) with
    | Error _ -> Assert.Pass()
    | Ok _ -> Assert.Fail("expected a validation error")

// ---- no constraints ----

type NoConstraints = ValidatedTypes.Provided.List<"{}">

[<Test>]
let ``no constraints: any list succeeds`` () =
    match NoConstraints.create([ box 1; box "two"; box 3.0 ]) with
    | Ok v -> CollectionAssert.AreEqual([ box 1; box "two"; box 3.0 ], v)
    | Error errs -> Assert.Fail(String.concat "; " errs)

[<Test>]
let ``no constraints: empty list succeeds`` () =
    match NoConstraints.create([]) with
    | Ok v -> Assert.That(v, Is.Empty)
    | Error errs -> Assert.Fail(String.concat "; " errs)

// ---- uniqueItems ----

type UniqueItems = ValidatedTypes.Provided.List<"{\"uniqueItems\": true}">

[<Test>]
let ``uniqueItems: all distinct values succeeds`` () =
    match UniqueItems.create([ box 1; box 2; box 3 ]) with
    | Ok v -> CollectionAssert.AreEqual([ box 1; box 2; box 3 ], v)
    | Error errs -> Assert.Fail(String.concat "; " errs)

[<Test>]
let ``uniqueItems: duplicate values fail`` () =
    match UniqueItems.create([ box 1; box 2; box 1 ]) with
    | Error _ -> Assert.Pass()
    | Ok _ -> Assert.Fail("expected a validation error")

[<Test>]
let ``uniqueItems: not set allows duplicates`` () =
    match NoConstraints.create([ box 1; box 1 ]) with
    | Ok v -> CollectionAssert.AreEqual([ box 1; box 1 ], v)
    | Error errs -> Assert.Fail(String.concat "; " errs)

// ---- combined constraints ----

type Bounded1To3Unique = ValidatedTypes.Provided.List<"{\"minItems\": 1, \"maxItems\": 3, \"uniqueItems\": true}">

[<Test>]
let ``combined: value within bounds and unique succeeds`` () =
    match Bounded1To3Unique.create([ box "a"; box "b" ]) with
    | Ok v -> CollectionAssert.AreEqual([ box "a"; box "b" ], v)
    | Error errs -> Assert.Fail(String.concat "; " errs)

[<Test>]
let ``combined: too short fails`` () =
    match Bounded1To3Unique.create([]) with
    | Error _ -> Assert.Pass()
    | Ok _ -> Assert.Fail("expected a validation error")

[<Test>]
let ``combined: too long fails`` () =
    match Bounded1To3Unique.create([ box "a"; box "b"; box "c"; box "d" ]) with
    | Error _ -> Assert.Pass()
    | Ok _ -> Assert.Fail("expected a validation error")

[<Test>]
let ``combined: duplicates fail`` () =
    match Bounded1To3Unique.create([ box "a"; box "a" ]) with
    | Error _ -> Assert.Pass()
    | Ok _ -> Assert.Fail("expected a validation error")
