namespace PrimitiveTypes

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

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


/// Define the weight of a nonnegative integer x to be the number of bits
/// that are set to 1 in its binary representation.
/// Fro example since 92 in base-2 equals 0b1011100, the weight of 92 is 4.
/// Write a program which takes as input a nonnegative integer x and their
/// difference, |y - x|, is as small as possible.
module ``Find a closest integer with then same weight`` =

    /// Smart solution is to swap k1 and k2 bits where the smallest k1 is
    /// the rightmost bit that's different from the LSB, and k2 must be
    /// the very next bit. In summary, the correct approach is to swap the
    /// two rightmost consecutive bits that differ.
    let rec logicV1 (n: int) i =
        if (n >>> i &&& 1) <> (n >>> (i + 1) &&& 1) then
            n ^^^ ((1 <<< i) ||| (1 <<< i + 1))
        else logicV1 n (i + 1)

    /// Solve the same problem with O(1) tome and space.
    let rec logicV2 (n: int) =
        if n &&& 1 = 0 then
            // 0b00010100 = n
            // 0b00010011 = n - 1
            // 0b11101100 = ~~~(n - 1)
            // 0b00000100 = ~~~(n - 1)) &&& n
            let k1 = (~~~(n - 1)) &&& n
            let k2 = k1 >>> 1
            n ^^^ k1 ^^^ k2
        else
            //0b00000111 = n
            //0b11111000 = ~~~n
            //0b11110111 = (~~~n) - 1
            //0b00001000 = ~~~((~~~n) - 1)
            let k1 = ~~~((~~~n) - 1)
            let k2 = k1 >>> 1
            n ^^^ k1 ^^^ k2
    
    [<TestClass>]
    type UnitTest () =
        
        let data = [
            0b00000001, 0b00000010
            0b00000011, 0b00000101
            0b00000111, 0b00001011
            0b00000010, 0b00000001
            0b00100100, 0b00100010
        ]
                
        [<TestMethod>]
        member this.Test () =
            for x, expected in data do
                (
                    let result = logicV1 x 0
                    Assert.IsTrue((expected = result))
                )
                (
                    let result = logicV2 x
                    Assert.IsTrue((expected = result))
                )

module ``Compute X x Y without arithmetical operations`` =
    let add (a: int, b: int) =
        let mutable carryin = 0
        let mutable ax, bx = a, b
        let mutable sum = 0
        let mutable i = 0
        while ax > 0 || bx > 0 do
            let ai, bi = ax &&& i, bx &&& i
            let carryout = (ai &&& bi) ||| (ai &&& carryin) ||| (bi &&& carryin)
            let k = ai ^^^ bi ^^^ carryin
            sum <- sum ||| k
            carryin <- carryout <<< 1
            i <- i <<< 1
            ax <- ax >>> 1
            bx <- bx >>> 1

    let multiply (x: int, y: int) =
        // 0b00000011 = 3
        // *
        // 0b00000101 = 5
        // ==========
        // 0b00000011 * 1
        // +
        // 0b00000000 = (0b00000011 << 1) * 0
        // +
        // 0b00001100 = (0b00000011 << 2) * 1
        // ==========
        // 0b00001111 = 15
        let mutable r = 0
        for i = 0 to 31 do
            if ((1 <<< i) &&& y) > 0 then
                r <- r + (x <<< i)
        r
                   
    [<TestClass>]
    type UnitTest () =
        
        let data = [
            3, 5, 15
        ]
                
        [<TestMethod>]
        member this.Test () =
            for x, y, expected in data do
                let result = multiply (x, y)
                Assert.IsTrue((expected = result))

module ``Compute X divide Y without arithmetical operations`` =

    let divide (x: int, y: int) =
        // 0b00001111 = 15
        // divide
        // 0b00000101 = 5
        // -> -> ->
        // 0b00001111 = 15
        /// -
        // 0b00001010 = 0b101 << 1
        // ===
        // 0b00000101
        // -
        // 0b00000101 = 0b101 << 0
        // 
        // result: 0b11 = 3
        let mutable sum = x
        let mutable power = 16
        let mutable yPower = y <<< power
        let mutable result = 0
        while sum >= y do
            while yPower > sum do
                power <- power - 1
                yPower <- yPower >>> 1

            sum <- sum - yPower
            result <- result ||| (1 <<< power)
        result
                    
    
    [<TestClass>]
    type UnitTest () =
        
        let data = [
            15, 5, 3
            25, 5, 5
            8, 3, 2
        ]
                
        [<TestMethod>]
        member this.Test () =
            for x, y, expected in data do
                let result = divide (x, y)
                Assert.IsTrue((expected = result))

module ``Compute X power Y`` =

    // 0b0011^5 = 0b0011^4 + 0b0011^1
    let divide (x: int, y: int) =
        let mutable product = 1
        let mutable xPower = x
        let mutable yPower = y
        while yPower > 0 do
            if (yPower &&& 1) > 0 then
                product <- product * xPower
            xPower <- xPower * xPower
            yPower <- yPower >>> 1

        product
                        
        
    [<TestClass>]
    type UnitTest () =
            
        let data = [
            2, 3, 8
            3, 2, 9
        ]
                    
        [<TestMethod>]
        member this.Test () =
            for x, y, expected in data do
                let result = divide (x, y)
                Assert.IsTrue((expected = result))

module ``Revert decimal digits`` =

    let revertDigits (n: int) =
        let mutable x = n
        let mutable r = 0
        while x > 0 do
            r <- r * 10 + (x % 10)
            x <- x / 10
        r
            
    [<TestClass>]
    type UnitTest () =
                
        let data = [
            42, 24
            5132, 2315
        ]
                        
        [<TestMethod>]
        member this.Test () =
            for n, expected in data do
                let result = revertDigits n
                Assert.IsTrue((expected = result))

module ``Check if a decimal integer is a palindrome`` =

    let testPalindrome (n: int) =
        if n = 0 then true
        else
            let mutable i, j = Math.Log10(float n) |> int, 0
            let mutable x = n
            let mutable palindrome = true
            while x > 0 && palindrome do
                let right = x % 10
                let topDigit = Math.Pow(10., float i) |> int
                let left = x / topDigit
                if left <> right then
                    palindrome <- false
                x <- x / topDigit / 10
            palindrome
                
    [<TestClass>]
    type UnitTest () =
                    
        let data = [
            0, true
            1, true
            7, true
            11, true
            121, true
            343, true
            3421243, true
            12, false
            100, false
        ]
                            
        [<TestMethod>]
        member this.Test () =
            for n, expected in data do
                let result = testPalindrome n
                Assert.IsTrue((expected = result))

module ``Cenerate uniform Random numbers`` =
    
    let generateUniformRandomNumberFromTo (random: unit -> bool) maxRetries (a: int, b: int) =
        let shift = b - a + 1
        if shift < 2 then failwith "A must be less than B."
        let entropy = Math.Log2(float shift)
        let entropyInt =
            let i = entropy |> int
            if entropy - (i |> float) > 0. then i + 1
            else i
        let generate() =
            let mutable r = 0
            for i = 0 to entropyInt do
                let k = if random() then 1 else 0
                r <- (r <<< 1) + k
            r
        let rec genUntill retryIter =
            if retryIter < maxRetries then
                let random = generate()
                if random < shift then a + random
                else genUntill (retryIter + 1)
            else
                a
        genUntill 0

                    
    [<TestClass>]
    type UnitTest () =
                        
        let data = [
            3, 5
        ]
                                
        [<TestMethod>]
        member this.Test () =
                
            for a, b in data do
                let random = Random(0)
                let randomBool() = if random.Next(2) = 0 then false else true
                let testsCount = 10000
                let statistics =
                    seq { 1 .. testsCount }
                    |> Seq.map (fun _ -> generateUniformRandomNumberFromTo randomBool 10 (a, b))
                    |> Seq.groupBy id
                    |> Seq.map (fun (k, v) -> Seq.length v)
                    |> Seq.toList
                Assert.IsTrue((statistics.Length = b - a + 1))
                for i in statistics do
                    let expected = float testsCount / float statistics.Length
                    let delta = expected - float i
                    let ratio = delta / expected
                    Assert.IsTrue(ratio <= 0.1)

module ``Rectangle intersection``=

    type Rect = {
        X: int
        Width: int
        Y: int
        High: int
    }

    let intersecOneD(a1, a2, b1, b2) =
        if a1 <= b1 then
            let dx = b1 - a1
            let aw = a2 - dx
            if aw > 0 then
                Some (b1, Math.Min(aw, b2))
            else None
        else
            let dx = a1 - b1
            let bw = b2 - dx
            if bw > 0 then
                Some (a1, Math.Min(bw, a2))
            else None

    let intersection (a: Rect, b: Rect)  =
        let xIntersection = intersecOneD (a.X, a.Width, b.X, b.Width)
        let yInteresection = intersecOneD (a.Y, a.High, b.Y, b.High)
        match xIntersection, yInteresection with
        | Some (x, width), Some (y, high) ->
            Some { X = x; Width = width; Y = y; High = high }
        | _ -> None
    
                        
    [<TestClass>]
    type UnitTest () =

        let d = { X = 1; Width = 2; Y= 1; High = 2 }
                            
        let data = [
            { d with X = 0; Width = 2 },
            { d with X = 3; Width = 1 },
            None

            { d with X = 1; Width = 3 },
            { d with X = 2; Width = 2},
            Some { d with X = 2; Width = 2 }

            { d with X = 1; Width = 3 },
            { d with X = 0; Width = 2},
            Some { d with X = 1; Width = 1 }
        ]
                                    
        [<TestMethod>]
        member this.Test () =
                    
            for a, b, expected in data do
                let result = intersection (a, b)
                Assert.IsTrue((result = expected))
    
