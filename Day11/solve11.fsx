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
let simplify_SE_SW_S_Movements (tm:TotalMovement) =
    if tm.SE > 0 && tm.SW > 0 then
        let steps = System.Math.Min(tm.SE, tm.SW)
        {tm with SE = tm.SE - steps; SW = tm.SW - steps; S = tm.S + steps}
    else
        tm

let simplify_NE_S_SE_Movements (tm:TotalMovement) =
    if tm.NE > 0 && tm.S > 0 then
        let steps = System.Math.Min(tm.NE, tm.S)
        {tm with NE= tm.NE - steps; S = tm.S - steps; SE = tm.SE + steps}
    else
        tm

let simplify_NW_S_SW_Movements (tm:TotalMovement) =
    if tm.NW > 0 && tm.S > 0 then
        let steps = System.Math.Min(tm.NW, tm.S)
        {tm with NW= tm.NW - steps; S = tm.S - steps; SE = tm.SE + steps}
    else
        tm

let simplify_NW_SW_N_Movements (tm:TotalMovement) =
    if tm.SW > 0 && tm.N > 0 then
        let steps = System.Math.Min(tm.SW, tm.N)
        {tm with SW= tm.SW - steps; N = tm.N - steps; NW = tm.NW + steps}
    else
        tm

let simplify_NE_SE_N_Movements (tm:TotalMovement) =
    if tm.SE > 0 && tm.N > 0 then
        let steps = System.Math.Min(tm.SE, tm.N)
        {tm with SE = tm.SE - steps; N = tm.N - steps; NE = tm.NE + steps}
    else
        tm

let simplify = 
    simplify_N_S_Movements
    >> simplify_NE_SW_Movements
    >> simplify_NW_SE_Movements
    >> simplify_NE_NW_N_Movements
    >> simplify_SE_SW_S_Movements
    >> simplify_NE_S_SE_Movements
    >> simplify_NW_S_SW_Movements
    >> simplify_NW_SW_N_Movements
    >> simplify_NE_SE_N_Movements

let summarize (tm:TotalMovement) = tm.N + tm.NE + tm.SE + tm.S + tm.SW + tm.NW 

let readInput =
    Path.Combine(__SOURCE_DIRECTORY__, "input11.txt")
    |> File.ReadAllText
    |> splitOnComma
    |> Array.toList

readInput
|> List.countBy id
|> List.map (fun (direction, steps) -> {direction = direction;steps = steps })
|> findTotalMovement
|> simplify
|> summarize

// Part 2
