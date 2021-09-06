namespace Arrays

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
                swap a i left
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
            //[ B; C; A ], [ B; C; A ]
            [ B; C; A; B; C; A ], [ B; B; A; A; C; C ]
        ]
    
        [<TestMethod>]
        member this.Test () =
            for source, expected in data do
                let field = Array.ofList source

                sort field
                let test = Enumerable.SequenceEqual(field, expected)
                Assert.IsTrue(test)