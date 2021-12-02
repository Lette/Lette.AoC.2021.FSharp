namespace Lib

module Day01 =

    open FParsec

    type Depth = Depth of int
    type Change = MoreDeep | Same | LessDeep

    let getChange (Depth d1) (Depth d2) =
        match sign (d1 - d2) with
        | -1 -> MoreDeep
        | 0  -> Same
        | 1  -> LessDeep
        | x  -> failwith (sprintf "Unknown retval from sign: %i" x)

    let getChangeL =
        function
        | d1 :: d2 :: [] -> getChange d1 d2
        | _              -> failwith "list has wrong length"

    let addDepths (Depth d1) (Depth d2) = Depth (d1 + d2)

    let parse data =
        let depthP   = pint32 |>> Depth
        let newlineP = pchar '\n'
        let rowsP    = sepBy depthP newlineP

        match run rowsP data with
        | Success (result, _, _) -> result
        | Failure (msg, _, _)    -> failwith msg

    let depths = lazy (parse Day01Data.data)

    let part1 () =
        depths.Value
        |> List.windowed 2
        |> List.map getChangeL
        |> List.where ((=) MoreDeep)
        |> List.length

    let part2 () =
        depths.Value
        |> List.windowed 3
        |> List.map (List.reduce addDepths)
        |> List.windowed 2
        |> List.map getChangeL
        |> List.where ((=) MoreDeep)
        |> List.length

    let meta =
        [
            problem 1 1 part1 1167
            problem 1 2 part2 1130
        ]
