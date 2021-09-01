namespace ElementsOfProgrammingInterview

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

module PrimitiveTypes =

    // Writing a progra to count the number of bits that are set to 1 in positive integer in a good way.
    module CountBits =
        let countBits (n: int) =
            let mutable x = n
            let mutable count = 0
            while x > 0 do
                if (x &&& 1) > 0 then
                    count <- count + 1
                x <- x >>> 1
            count

        [<TestClass>]
        type UnitTest () =

            let data = [
                0b0, 0
                0b1, 1
                0b1, 1
                0b101, 2
            ]
    
            [<TestMethod>]
            member this.Test () =
                for x, count in data do
                    Assert.IsTrue(countBits x = count)

    module ParityOfAWord =
        let testWordParity_bruteForce (n: int) =
            let mutable x = n
            let mutable result = 0
            while x > 0 do
                result <- result ^^^ (x &&& 1)
                x <- x >>> 1
            result

        /// Erasing the lowest set tib in a word in a single operation.
        /// x & (x - 1) equals x with its lowest set bit erased.
        /// For example if x = 0b00101100 then x - 1 = (0b00101011), so x & (x - 1) = 0b00101000
        let testWordParityMathTrick (n: int) =
            let mutable x = n
            let mutable result = 0
            while x > 0 do
                result <- result ^^^ 1
                x <- x &&& (x - 1)
            result

        // Constructs 16bit words parity test results cached.
        let buildWordParityTestCached() =
            let int16Combinations = (1 <<< 16)
            let int16Max = int16Combinations - 1
            let cache = Array.init int16Combinations testWordParityMathTrick
            // O(1)
            fun (x: int) ->
                let l1 = cache.[x &&& int16Max]
                let l2 = cache.[x >>> 16 &&& int16Max]
                l1 ^^^ l2

        // O(16)
        let testWordParityByXorPars (n: int) =
            let int16Max = (1 <<< 16) - 1
            let l1 = n &&& int16Max
            let l2 = n >>> 16 &&& int16Max
            let l = l1 ^^^ l2
            testWordParityMathTrick l

        // O(1)
        let testWordParityByXorPower (n: int) =
            let mutable x = n
            x <- x ^^^ (x >>> 16)
            x <- x ^^^ (x >>> 8)
            x <- x ^^^ (x >>> 4)
            x <- x ^^^ (x >>> 2)
            x <- x ^^^ (x >>> 1)
            x &&& 0b1


        [<TestClass>]
        type UnitTest () =

            let data = [
                0b1011, 1
                0b10001000, 0
                0b00101100, 1
            ]
    
            [<TestMethod>]
            member this.Test () =
                for x, parity in data do
                    Assert.IsTrue(testWordParity_bruteForce x = parity)
                    Assert.IsTrue(testWordParityMathTrick x = parity)
                    Assert.IsTrue(buildWordParityTestCached() x = parity)
                    Assert.IsTrue(testWordParityByXorPars x = parity)
                    Assert.IsTrue(testWordParityByXorPower x = parity)

    /// Turns 0b01010000 to 0b01011111
    module ``Right Propagate The Rightmost Set Bit`` =
        let bruteForce (x: int) =
            let mutable n = 1
            while x ||| n <> x do
                n <- n <<< 1
            if n = 1 then x
            else
                x ||| (n - 1)

        let smart (x: int) =
            // 0b01010000 = x
            // 0b01001111 = x - 1
            // 0b01011111 = x ||| (x - 1)
            x ||| (x - 1)
                

        [<TestClass>]
        type UnitTest () =

            let data = [
                0b01010000, 0b01011111
            ]
        
            [<TestMethod>]
            member this.Test () =
                for x, result in data do
                    Assert.IsTrue(bruteForce x = result)
                    Assert.IsTrue(smart x = result)

    /// Compute x mod a power of two e.g, returns 13 for 77 mod 64.
    module ``Compute X MOD 64`` =
        let logic (x: int) =
            // Powers of 2 like 64 are single bit in a number: 0b001000000
            x &&& 64 - 1
                

        [<TestClass>]
        type UnitTest () =

            let data = [
                77, 13
            ]
        
            [<TestMethod>]
            member this.Test () =
                for x, result in data do
                    Assert.IsTrue(logic x = result)

    // Power of 2 are single bit in a number: 0b001000000
    module ``Test if X is power of 2`` =

        let bruteForce (n: int) =
            if n = 0 then true
            else
                let mutable i = 0
                let mutable any = true
                while i < 31 && any do
                    if ((n ^^^ (1 <<< i)) = 0) then
                        any <- false
                    else
                        i <- i + 1
                not any

        let smart (n: int) =
            let k = (n - 1) &&& n
            k = 0

        [<TestClass>]
        type UnitTest () =

            let data = [
                1, true
                2, true
                4, true
                8, true
                16, true
                7, false
                33, false
            ]
        
            [<TestMethod>]
            member this.Test () =
                for x, result in data do
                    Assert.IsTrue(bruteForce x = result)
                    Assert.IsTrue(smart x = result)

    module ``Get lowest bit set`` =

        let logic (n: int) = ~~~(n - 1) &&& n

        [<TestClass>]
        type UnitTest () =

            let data = [
                0b010000, 0b010000
                0b001011, 0b000001
                0b010100, 0b000100
            ]
        
            [<TestMethod>]
            member this.Test () =
                for x, result in data do
                    Assert.IsTrue(logic x = result)

    module ``Swap two bits by indexes`` =
    
        let logic (n: int) i j =
            let iBitSet = n &&& (1 <<< i) > 0
            let jBitSet = n &&& (1 <<< j) > 0
            if iBitSet = jBitSet then n
            else n ^^^ (1 <<< i) ^^^ (1 <<< j)

        [<TestClass>]
        type UnitTest () =
    
            let data = [
                0b01001001, 1, 6, 0b00001011
            ]
            
            [<TestMethod>]
            member this.Test () =
                for x, i, j, expected in data do
                    let result = logic x i j
                    Assert.IsTrue((expected = result))

    /// Write a program that takes 32 bit integer and returns the 32-bit unsigned integer
    /// consisting of the bits of the input in reverse order.
    module ``Invert bits`` =
        
            let buildReverseBits () =

                let int16Combinations = 1 <<< 16

                let int16Mask = uint int16Combinations - 1u

                let revertInt16(n: uint16) =
                    let mutable x = n
                    for i = 0 to 7 do
                        let altIndex = 15 - i
                        if ((x >>> i &&& 1us) ^^^ (x >>> altIndex &&& 1us)) > 0us then
                            x <- x ^^^ (1us <<< i) ^^^ (1us <<< altIndex)
                    x

                let cache = Array.init int16Combinations (uint16 >> revertInt16)
                fun (n: uint) -> 
                    let p1 = n &&& int16Mask
                    let p2 = n >>> 16 &&& int16Mask
                    let p1i = uint cache.[int p1]
                    let p2i = uint cache.[int p2]
                    p1i <<< 16 ||| p2i
    
            [<TestClass>]
            type UnitTest () =
        
                let data = [
                    0b0000_0000_0100_0000_0110_1000_0100_1001u, 0b1001_0010_0001_0110_0000_0010_0000_0000u
                ]
                
                [<TestMethod>]
                member this.Test () =
                    let reverseBits = buildReverseBits()
                    for x, expected in data do
                        let result = reverseBits x
                        Assert.IsTrue((expected = result))

    
