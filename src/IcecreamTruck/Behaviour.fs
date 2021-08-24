module Behaviour

open Domain
open Projections

let sellFlavour flavour (events: Event list) = 
    let stock = 
        events 
        |> project flavoursInStock 
        |> stockOfFlavour flavour

    match stock with
    | 0 -> [FlavourWasNotInStock flavour]
    | 1 -> [FlavourSold flavour; FlavourWentOutOfStock flavour]
    | _ -> [FlavourSold flavour]

let restockFlavour flavour number (_: Event list) = 
    [FlavourRestocked (flavour, number)]