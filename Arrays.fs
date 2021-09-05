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
                if leftRed > i then
                    i <- leftRed
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