let input = 277678

type Direction =
| E
| N
| W
| S

type Position = {
    x: int
    y: int
}

type State = {
    maxE: int
    maxN: int
    minW: int
    minS: int
    position: Position
    direction: Direction
}

let start = {
    maxE = 0
    maxN = 0
    minW = 0
    minS = 0
    position = {x=0;y=0}
    direction = E}

let move (state:State) _ =
    match state.direction with
    | E -> 
        if state.position.x > state.maxE then
            {state with maxE = state.position.x; direction = N; position = {x = state.position.x; y = state.position.y + 1}}
        else
            {state with position = {x = state.position.x + 1; y = state.position.y}}
    | N -> 
        if state.position.y > state.maxN then
            {state with maxN = state.position.y; direction = W; position = {x = state.position.x - 1; y = state.position.y}}
        else
            {state with position = {x = state.position.x; y = state.position.y + 1}}
    | W -> 
        if state.position.x < state.minW then
            {state with minW = state.position.x; direction = S; position = {x = state.position.x; y = state.position.y - 1}}
        else
            {state with position = {x = state.position.x - 1; y = state.position.y}}
    | S -> 
        if state.position.y < state.minS then
            {state with minS = state.position.y; direction = E; position = {x = state.position.x + 1; y = state.position.y}}
        else
            {state with position = {x = state.position.x; y = state.position.y - 1}}

let abs (i:int) = System.Math.Abs(i)

let distance state =
    (abs state.position.x) + (abs state.position.y)

seq {2 .. input} 
|> Seq.fold move start 
|> distance


// Part 2

type Sum = {
    state: State
    sum: int
}

let sumStart = {
    state = start
    sum = 1
}

let matchPosition (p:Position) (s:Sum) =
    if s.state.position = p then 
        Some s.sum
    else
        None 

let findSumAtPosition (l:Sum list) (pos:Position) =
    l |> List.choose (matchPosition pos) |> List.sum

let calculateSum (l:Sum list) (pos:Position) =
    findSumAtPosition l {x = pos.x + 1; y = pos.y}
    + findSumAtPosition l {x = pos.x + 1; y = pos.y + 1}
    + findSumAtPosition l {x = pos.x; y = pos.y + 1}
    + findSumAtPosition l {x = pos.x - 1; y = pos.y + 1}
    + findSumAtPosition l {x = pos.x - 1; y = pos.y}
    + findSumAtPosition l {x = pos.x - 1; y = pos.y - 1}
    + findSumAtPosition l {x = pos.x; y = pos.y - 1}
    + findSumAtPosition l {x = pos.x + 1; y = pos.y - 1}

let findNextSum (lst:Sum list) =
    let current = List.head lst
    let nextState = move current.state 0
    let sumHere = calculateSum lst nextState.position
    {
        state = nextState
        sum = sumHere
    } :: lst

let rec solve (lst:Sum list) =
    let newLst = lst |> findNextSum
    match newLst with
    | [] -> 0
    | h::_ when h.sum > input -> h.sum
    | _ -> solve newLst

solve [sumStart]