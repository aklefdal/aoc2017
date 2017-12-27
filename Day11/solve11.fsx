open System.IO

let splitOnComma (s:string) = s.Split([|','|]) 

type Movement = {
    direction: string
    steps: int
}

type TotalMovement = {
    N: int
    NE: int
    SE: int
    S: int
    SW: int
    NW: int
}

let findSteps (direction:string) (movements:Movement list) =
    movements 
    |> List.find (fun movement -> movement.direction = direction) 
    |> (fun movement -> movement.steps)

let findTotalMovement (movements:Movement list) =
    {
        N = movements |> findSteps "n"
        NE = movements |> findSteps "ne"
        SE = movements |> findSteps "se"
        SW = movements |> findSteps "s"
        S = movements |> findSteps "sw"
        NW = movements |> findSteps "nw"
    }

let simplify_N_S_Movements (tm:TotalMovement) =
    if tm.N > tm.S then
        {tm with N = tm.N - tm.S; S = 0}
    else
        {tm with N = 0; S = tm.S - tm.N}

let simplify_NW_SE_Movements (tm:TotalMovement) =
    if tm.NW > tm.SE then
        {tm with NW = tm.NW - tm.SE; SE = 0}
    else
        {tm with NW = 0; SE = tm.SE - tm.NW}

let simplify_NE_SW_Movements (tm:TotalMovement) =
    if tm.NE > tm.SW then
        {tm with NE = tm.NE - tm.SW; SW = 0}
    else
        {tm with NE = 0; SW = tm.SW - tm.NE}

let simplify_NE_NW_N_Movements (tm:TotalMovement) =
    if tm.NE > 0 && tm.NW > 0 then
        let steps = System.Math.Min(tm.NE, tm.NW)
        {tm with NE = tm.NE - steps; NW = tm.NW - steps; N = tm.N + steps}
    else
        tm

let summarize (tm:TotalMovement) = tm.N + tm.NE + tm.SE + tm.S + tm.SW + tm.NW 

Path.Combine(__SOURCE_DIRECTORY__, "input11.txt")
|> File.ReadAllText
|> splitOnComma
|> Array.toList
|> List.countBy id
|> List.map (fun (direction, steps) -> {direction = direction;steps = steps })
|> findTotalMovement
|> simplify_N_S_Movements
|> simplify_NE_SW_Movements
|> simplify_NW_SE_Movements
|> simplify_NE_NW_N_Movements
|> summarize