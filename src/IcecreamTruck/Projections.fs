module Projections

open Domain

type Projection<'State, 'Event> = {
    Init: 'State
    Update: 'State -> 'Event -> 'State
}

let project (projection: Projection<_, _>) events = 
    events |> List.fold projection.Update projection.Init

let soldOfFlavour flavour state = 
    state 
    |> Map.tryFind flavour 
    |> Option.defaultValue 0 

let updateSoldFlavours state event = 
    match event with
    | FlavourSold flavour -> 
        state
        |> soldOfFlavour flavour
        |> fun portions -> state |> Map.add flavour (portions + 1)

    | _ -> state

let soldFlavours: Projection<Map<Flavour, int>, Event> = {
    Init = Map.empty
    Update = updateSoldFlavours
}

let restock flavour number stock = 
    stock
    |> Map.tryFind flavour
    |> Option.defaultValue 0
    |> fun portions -> stock |> Map.add flavour (portions + number)

let updateFlavoursInStock stock event = 
    match event with
    | FlavourSold flavour ->
        stock |> restock flavour -1

    | FlavourRestocked (flavour, number) ->
        stock |> restock flavour number

    | _ -> stock

let flavoursInStock: Projection<Map<Flavour, int>, Event> = {
    Init = Map.empty
    Update = updateFlavoursInStock
}

let stockOfFlavour flavour stock =
    stock
    |> Map.tryFind flavour
    |> Option.defaultValue 0