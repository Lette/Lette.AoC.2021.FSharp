namespace App

open Lib

module Runner =

    let days =
        [
            Day01.meta
            Day02.meta
        ]

    let run () =
        days
        |> List.collect id
        |> List.map (fun p -> { p with Result = p.Runner (); })

module app =

    [<EntryPoint>]
    let main _ =

        let formatResult (p : Problem) =
            let result =
                if p.Result = p.Expected then
                    "Great success!"
                else
                    sprintf "Fail. (Expected: %i)" p.Expected

            sprintf "Day %02i, part %i: %i    %s" p.Day p.Part p.Result result

        Runner.run ()
        |> List.iter (formatResult >> (printfn "%s"))

        0
