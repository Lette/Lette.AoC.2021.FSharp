namespace Lib

module Day03 =

    open FParsec

    let parse data =

        let digitsP = many (digit |>> (string >> int))
        let newlineP = pchar '\n'
        let rowsP = sepBy digitsP newlineP

        match run rowsP data with
        | Success (result, _, _) -> result
        | Failure (msg, _, _)    -> failwith msg

    let data = lazy (parse Day03Data.data)

    let part1 () =
        let length = data.Value |> List.length
        let halfLength = length / 2

        let gamma =
            data.Value
            |> List.reduce (List.map2 (+))
            |> List.map (fun x -> if x > halfLength then 1 else 0)
            |> List.fold (fun s x -> s * 2 + x) 0

        let epsilon = pown 2 (data.Value |> List.head |> List.length) - 1 - gamma

        gamma * epsilon

    let part2 () =

        let rating comparison xss =

            let rec inner rest acc =
                match rest with
                | []      -> acc |> List.rev
                | x :: [] -> (acc |> List.rev) @ x
                | _       ->
                    let (ones, zeroes) = rest |> List.partition (fun xs -> List.head xs = 1)
                    if comparison (List.length ones) (List.length zeroes) then
                        inner (ones |> List.map (List.tail)) (1 :: acc)
                    else
                        inner (zeroes |> List.map (List.tail)) (0 :: acc)

            inner xss []
            |> List.fold (fun s x -> s * 2 + x) 0

        let oxygenGeneratorRating = rating (>=) data.Value
        let co2ScrubberRating = rating (<) data.Value

        oxygenGeneratorRating * co2ScrubberRating

    let meta =
        [
            problem 3 1 part1 1458194
            problem 3 2 part2 2829354
        ]
