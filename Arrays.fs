﻿namespace Arrays

open System
open System.Linq
open Microsoft.VisualStudio.TestTools.UnitTesting

module ``Sort odd numbers to left and even numbers to right`` =
    let sortOddEven (a: int array) =
        let mutable i, j = 0, a.Length - 1
        while i < j do
            if a.[i] % 2 = 1 then
                i <- i + 1
            else
                let t = a.[i]
                a.[i] <- a.[j]
                a.[j] <- t
                j <- j - 1
        

    [<TestClass>]
    type UnitTest () =

        let data = [
            [| 1; 3; 5 |], 5
            [| 0; 2; 4 |], -1
            [| 0; 1; 2; 3; 4; 5 |], 2
        ]
    
        [<TestMethod>]
        member this.Test () =
            for source, maxOddIndex in data do
                let field = Array.copy source
                sortOddEven field
                for i = 0 to field.Length - 1 do
                    let odd = field.[i] % 2 > 0
                    let correct = odd = (i <= maxOddIndex)
                    Assert.IsTrue(correct)

module ``Duch national flag sorting`` =

    type Colour = Red | White | Blue

    let swap (a: 'a array) i j = 
        let t = a.[i]
        a.[i] <- a.[j]
        a.[j] <- t

    let sort (a: Colour array) =
        let mutable i, leftRed, rightBlue = 0, 0, a.Length - 1
        while i <= rightBlue do
            match a.[i] with
            | Red ->
                swap a i leftRed
                leftRed <- leftRed + 1
                i <- i + 1
            | White ->
                i <- i + 1
            | Blue ->
                swap a i rightBlue
                rightBlue <- rightBlue - 1
        ()

        

    [<TestClass>]
    type UnitTest () =

        let data = [
            [| White; Blue; Red |], [| Red; White; Blue |]
            [| Red; White; Red; Blue; Red; White; Blue; White; Red  |], [| Red; Red; Red; Red; White; White; White; Blue; Blue |]
        ]
    
        [<TestMethod>]
        member this.Test () =
            for source, expected in data do
                let field = Array.copy source

                sort field
                let test = Enumerable.SequenceEqual(field, expected)
                Assert.IsTrue(test)


module ``3 Key Groups`` =

    type Value = A | B | C

    let swap (a: 'a array) i j = 
        let t = a.[i]
        a.[i] <- a.[j]
        a.[j] <- t

    /// Assuming that keys take one of tree values, reorder the array so that all objects
    /// with the same key appear together. Ther order of the subarrays is not important.

    let sort (a: Value array) =
        let typeLeft = a.[0]
        let mutable i, left, right = 1, 1, a.Length - 1

        let swapRight() =
            swap a i right
            right <- right - 1

        /// Mature strategy uses left type from first element and
        /// paremetrized right type.
        let matureStrategy (typeRight: Value) () =
            let item = a.[i]
            if item = typeLeft then
                swap a i left
                left <- left + 1
                i <- i + 1
            elif item = typeRight then swapRight()
            else 
                i <- i + 1

        let mutable strategy: unit -> unit = id

        /// We know left type and any non-left type first time detected will be
        /// identified as right type. Strategy will be updates to work with three types.
        let initialStrategy() =
            let item = a.[i]
            if item = typeLeft then
                left <- left + 1
                i <- i + 1
            else
                swapRight()
                strategy <- matureStrategy item

        strategy <- initialStrategy

        while i <= right do
            strategy()


    [<TestClass>]
    type UnitTest () =

        let data = [
            [ B; C; A ], [ B; A; C ]
            [ B; C; A; B; C; A ], [ B; B; A; A; C; C ]
        ]
    
        [<TestMethod>]
        member this.Test () =
            for source, expected in data do
                let field = Array.ofList source

                sort field
                let test = Enumerable.SequenceEqual(field, expected)
                Assert.IsTrue(test)

/// Given an aray A of n objects with keys that takes one of four values,
/// reorder the array so that all objects that have the key appear together.
module ``Four different keys appear together`` =

    type Value = L1 | L2 | R1 | R2

    let sort (a: Value array) =

        let swap i j = 
            let t = a.[i]
            a.[i] <- a.[j]
            a.[j] <- t

        let typeLeft1 = a.[0]
        let mutable left1, left2, right1, right2 = 1, 1, a.Length - 1, a.Length - 1

        let mutable strategy: unit -> unit = ignore

        // Stage when all four types are defined
        let stageMature typeLeft2 typeRight1 () =
            let item = a.[left2]
            if item = typeLeft1 then
                swap left1 left2
                left1 <- left1 + 1
                if left1 > left2 then
                    left2 <- left1
            else if item = typeLeft2 then
                left2 <- left2 + 1
            else if item = typeRight1 then
                swap left2 right1
                right1 <- right1 - 1
                if right1 < right2 then
                    right2 <- right1
            else
                swap left2 right2
                right2 <- right2 - 1

        // Stage when typeLeft1 and typeLeft2 only are initialized
        let stageLeft2 typeLeft2 () =
            let item = a.[left2]
            if item = typeLeft1 then
                left1 <- left1 + 1
                left2 <- left1
            else if item = typeLeft2 then
                left2 <- left2 + 1
            else
                swap left2 right1
                right1 <- right1 - 1
                if right1 < right2 then
                    right2 <- right1
                strategy <- stageMature typeLeft2 item

        // Stage when typeLeft1 only is initialized
        let stageLeft1() =
            let item = a.[left2]
            if item = typeLeft1 then
                left1 <- left1 + 1
                left2 <- left2 + 1
            else
                left2 <- left2 + 1
                strategy <- stageLeft2 item

        strategy <- stageLeft1

        while left2 <= right2 do
            strategy()


    [<TestClass>]
    type UnitTest () =

        let data = [
            [ L1; L2; R1; R2 ], [ L1; L2; R2; R1 ]

            [ L1; L1; L1; L2; L2; L2; R1; R1; L1; L2; L1; R1; R2; L1; R2; L2; R2 ],
            [ L1; L1; L1; L1; L1; L1; L2; L2; L2; L2; L2; R2; R2; R2; R1; R1; R1 ]
        ]

        [<TestMethod>]
        member this.Test () =
            for source, expected in data do
                let field = Array.ofList source

                sort field
                let test = Enumerable.SequenceEqual(field, expected)
                Assert.IsTrue(test)

module ``Sort bool`` =

    let swap (a: 'a array) i j = 
        let t = a.[i]
        a.[i] <- a.[j]
        a.[j] <- t

    let sort (a: bool array) =
        let mutable i, j = 0, a.Length - 1
        while i <= j do
            match a.[i] with
            | false ->
                i <- i + 1
            | true ->
                swap a i j
                j <- j - 1

    [<TestClass>]
    type UnitTest () =

        let data = [
            [ false; true; false; true ], [ false; false; true; true ]
            [ true; true; false; true; false; true ], [ false; false; true; true; true; true]
        ]
    
        [<TestMethod>]
        member this.Test () =
            for source, expected in data do
                let field = List.toArray source

                sort field
                let test = Enumerable.SequenceEqual(field, expected)
                Assert.IsTrue(test)

/// Given an array A of n objects with Boolean-valued keys,
/// reorder the array so that objects that have the key false appear first.
/// The relative ordering of the objects with key true should not change.
module ``Sort bool with true order`` =

    type Item = { Tag: string; Key: bool }

    let swap (a: 'a array) i j = 
        let t = a.[i]
        a.[i] <- a.[j]
        a.[j] <- t

    let sort (a: Item array) =
        let mutable i, j = a.Length - 1, a.Length - 1
        while i >= 0 do
            if a.[i].Key then
                swap a i j
                j <- j - 1
            i <- i - 1

    [<TestClass>]
    type UnitTest () =

        let data = [
            [
                { Tag = "A"; Key = false }
                { Tag = "B"; Key = true }
                { Tag = "C"; Key = false }
                { Tag = "D"; Key = true }
            ],
            [
                "B"
                "D"
            ]

            [
                { Tag = "A"; Key = true }
                { Tag = "B"; Key = true }
                { Tag = "C"; Key = false }
                { Tag = "D"; Key = true }
                { Tag = "E"; Key = false }
                { Tag = "F"; Key = true }
            ],
            [
                "A"
                "B"
                "D"
                "F"
            ]
        ]
    
        [<TestMethod>]
        member this.Test () =
            for source, expected in data do
                let field = List.toArray source

                sort field
                let fieldKey =
                    field |> Array.map (fun i -> i.Tag)
                    |> Array.rev |> Array.take expected.Length |> Array.rev
                let test = Enumerable.SequenceEqual(fieldKey, expected)
                Assert.IsTrue(test)