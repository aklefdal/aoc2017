
let rec gen (q:int64) (p:int64) (count:int) = seq { 
    let m = p * q
    let n = m % 2147483647L
    yield n
    if count > 0 then 
        yield! gen q n (count - 1)
}

let getLower16Bits (l:int64) =
    l % (256L * 256L)

let pairGetLower16Bits (a, b) = 
    (getLower16Bits a, getLower16Bits b)

Seq.zip (gen 16807L 512L (40 * 1000 * 1000)) (gen 48271L 191L (40 * 1000 * 1000))
|> Seq.map pairGetLower16Bits
|> Seq.filter (fun (a, b) -> a = b)
|> Seq.length

// Part 2

let rec gen2 (w:int64) (q:int64) (p:int64) (count:int) = seq { 
    let rec innerGenB p =
        let m = p * q
        let n = m % 2147483647L
        match n % w with
        | 0L -> n
        | _ -> innerGenB n

    let n = innerGenB p
    yield n
    if count > 1 then 
        yield! gen2 w q n (count - 1)
}

Seq.zip (gen2 4L 16807L 512L (5 * 1000 * 1000)) (gen2 8L 48271L 191L (5 * 1000 * 1000)) 
|> Seq.map pairGetLower16Bits
|> Seq.filter (fun (a, b) -> a = b)
|> Seq.length

