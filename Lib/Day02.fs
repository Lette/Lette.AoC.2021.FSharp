namespace Lib

module Day02 =

    open FParsec

    type Move =
        | Forward of int
        | Up of int
        | Down of int

    type Position = { Horizontal: int; Depth: int; Aim: int; } with
        static member Zero = { Horizontal = 0; Depth = 0; Aim = 0; }

    let go1 pos move =
        match move with
        | Forward x -> { pos with Horizontal = pos.Horizontal + x; }
        | Up x      -> { pos with Depth = pos.Depth - x; }
        | Down x    -> { pos with Depth = pos.Depth + x; }

    let go2 pos move =
        match move with
        | Forward x -> { pos with Horizontal = pos.Horizontal + x; Depth = pos.Depth + pos.Aim * x; }
        | Up x      -> { pos with Aim = pos.Aim - x; }
        | Down x    -> { pos with Aim = pos.Aim + x; }

    let parse data =
        let forwardP = pstring "forward " >>. pint32 |>> Forward
        let upP      = pstring "up "      >>. pint32 |>> Up
        let downP    = pstring "down "    >>. pint32 |>> Down
        let moveP    = forwardP <|> upP <|> downP
        let newlineP = pchar '\n'
        let rowsP    = sepBy moveP newlineP

        match run rowsP data with
        | Success (result, _, _) -> result
        | Failure (msg, _, _)    -> failwith msg

    let moves = lazy (parse (Day02Data.data))

    let part1 () =
        moves.Value
        |> List.fold go1 Position.Zero
        |> (fun p -> p.Horizontal * p.Depth)

    let part2 () =
        moves.Value
        |> List.fold go2 Position.Zero
        |> (fun p -> p.Horizontal * p.Depth)

    let meta =
        [
            problem 2 1 part1 1636725
            problem 2 2 part2 1872757425
        ]
