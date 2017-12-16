let rec genANext (p:int64) (count:int) = seq { 
    let m = p * 16807L
    let n = m % 2147483647L
    yield n
    if count > 0 then 
        yield! genANext n (count - 1)
}

let rec genBNext (p:int64) (count:int) = seq { 
    let m = p * 48271L
    let n = m % 2147483647L
    yield n
    if count > 0 then 
        yield! genBNext n (count - 1)
}

let aStart = 512L
let bStart = 191L

let getLower16Bits (l:int64) =
    l % (256L * 256L)

let pairGetLower16Bits (a, b) = 
    (getLower16Bits a, getLower16Bits b)
let count = 40 * 1000 * 1000

let countEquals p =
    p
    |> Seq.map pairGetLower16Bits
    |> Seq.filter (fun (a, b) -> a = b)
    |> Seq.length

Seq.zip (genANext aStart count) (genBNext bStart count) 
|> countEquals
