namespace Lib

type Problem = { Day: int; Part: int; Runner: unit -> int; Result: int; Expected: int; }

[<AutoOpen>]
module Problem =

    let problem day part runner expected =
        { Day = day; Part = part; Runner = runner; Result = 0; Expected = expected; }
