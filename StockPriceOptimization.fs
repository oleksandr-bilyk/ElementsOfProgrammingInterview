// Devide and conquire method to find optimized stock buy-sell price.

namespace ElementsOfProgrammingInterview

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

module StockPriceOptimization =

    let min (array: int array) startIndex endIndex =
        let mutable min = array.[startIndex]
        for i = (startIndex + 1) to endIndex do
            let item = array.[i]
            if item < min then min <- item
        min

    let max (array: int array) startIndex endIndex =
        let mutable max = array.[startIndex]
        for i = (startIndex + 1) to endIndex do
            let item = array.[i]
            if item > max then max <- item
        max

    let rec maximizeProfit array startIndex endIndex =
        let indexDif = endIndex - startIndex
        if indexDif < 1 then 0
        else
            let leftEndIndex = startIndex + indexDif / 2
            let profitMaximizedParts =
                let profitMaximizedLeft = maximizeProfit array startIndex leftEndIndex
                let prifitMaximizedRight = maximizeProfit array (leftEndIndex + 1) endIndex
                Math.Max(profitMaximizedLeft, prifitMaximizedRight)
            let profitMaximizedFull =
                let leftMin = min array startIndex leftEndIndex
                let rightMax = max array (leftEndIndex + 1) endIndex
                if leftMin < rightMax then rightMax - leftMin
                else 0
            Math.Max(profitMaximizedParts, profitMaximizedFull)


[<TestClass>]
type TestStockPriceOptimization () =

    [<TestMethod>]
    member this.Test () =
        let testCases = [
            [| 1; 5; 3; 2 |], 4
            [| 7; 2; 8; 9 |], 7
            [| 1; 6; 7; 9 |], 8
            [| 9; 7; 4; 1 |], 0
            [| 10; 5; 1; 0 |], 0
        ]
        for data, expectedProfitMaximized in testCases do
            let profitMaximized = StockPriceOptimization.maximizeProfit data 0 (data.Length - 1)
            Assert.IsTrue((expectedProfitMaximized = profitMaximized))
       
