module Helper

open Projections

let printUl list = 
    list
    |> List.iteri (fun i item -> printfn " %i: %A" (i + 1) item)

let printEvents header events = 
    events
    |> List.length 
    |> printfn "History for %s (Length: %i)" header

    events |> printUl

let printTotalHistory history =
    history
    |> Map.fold (fun length _ events -> length + (events |> List.length)) 0
    |> printfn "Total History Length: %i"

let printSoldFlavour flavour state = 
    state 
    |> soldOfFlavour flavour 
    |> printfn "Sold %A: %i" flavour

let printStockOfFlavour flavour stock =
    stock
    |> stockOfFlavour flavour
    |> printfn "Stock %A: %i" flavour