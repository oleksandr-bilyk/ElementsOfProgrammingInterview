namespace ElementsOfProgrammingInterview

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

module QuickSort =
    

    // 1; 3; 5; 2; 4; 6 -> 1; 2; 3; 4; 5; 6
    // 1; 2; 3; 4; 5; 6
    let merge (array: int array) startIndex endIndex median =
        let mutable i, j = startIndex, (median + 1)
        while j <= endIndex do
            if array.[i] > array.[j] then
                let t = array.[i]
                array.[i] <- array.[j]
                array.[j] <- t

            i <- i + 1

            if i = j then
                j <- j + 1


    let rec quicSort (array: int array) startIndex endIndex =
        if startIndex >= endIndex then ()
        else
            let median = startIndex + (endIndex - startIndex) / 2
            quicSort array startIndex median
            quicSort array (median + 1) endIndex
            merge array startIndex endIndex median



[<TestClass>]
type TestQuickSort () =

    [<TestMethod>]
    member this.Test () =
        let testCases = [
            [1; 3; 5; 2; 4; 6 ], [1; 2; 3; 4; 5; 6]
            [ 2; 3; 1], [1; 2; 3]
            [ 5; 10; 3; 4; 7; 14 ], [ 3; 4; 5; 7; 10; 14]
        ]
        for data, expectedResult in testCases do
            let source = List.toArray data
            QuickSort.quicSort source 0 (source.Length - 1)
            Assert.IsTrue(System.Linq.Enumerable.SequenceEqual(source, expectedResult))
       
