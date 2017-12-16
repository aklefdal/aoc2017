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


// Part 2

let rec genA4Next (p:int64) (count:int) = seq { 
    let rec innerGenA p =
        let m = p * 16807L
        let n = m % 2147483647L
        match n % 4L with
        | 0L -> n
        | _ -> innerGenA n

    let n = innerGenA p
    yield n
    if count > 1 then 
        yield! genA4Next n (count - 1)
}

let rec genB8Next (p:int64) (count:int) = seq { 
    let rec innerGenB p =
        let m = p * 48271L
        let n = m % 2147483647L
        match n % 8L with
        | 0L -> n
        | _ -> innerGenB n

    let n = innerGenB p
    yield n
    if count > 1 then 
        yield! genB8Next n (count - 1)
}

let count2 = 5 * 1000 * 1000

Seq.zip (genA4Next aStart count2) (genB8Next bStart count2) 
|> countEquals

