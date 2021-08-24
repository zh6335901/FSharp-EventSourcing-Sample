open EventStore
open Domain
open Helper
open Projections

[<EntryPoint>]
let main argv = 
    let eventStore : EventStore<Event> = EventStore.initialize();

    let truck1 = System.Guid.NewGuid();
    let truck2 = System.Guid.NewGuid();

    eventStore.Evolve truck1 (Behaviour.sellFlavour Vanilla)
    eventStore.Evolve truck1 (Behaviour.sellFlavour Strawberry)
    eventStore.Evolve truck1 (Behaviour.restockFlavour Vanilla 3)
    eventStore.Evolve truck1 (Behaviour.sellFlavour Vanilla)

    eventStore.Evolve truck2 (Behaviour.sellFlavour Vanilla)

    let eventsTruck1 = eventStore.GetStream truck1
    let eventsTruck2 = eventStore.GetStream truck2

    eventsTruck1 |> printEvents " Truck1"
    eventsTruck2 |> printEvents " Truck2"

    let sold: Map<Flavour, int> = 
        eventsTruck1 |> project soldFlavours

    printSoldFlavour Vanilla sold
    printSoldFlavour Strawberry sold

    let stock = 
        eventsTruck1 |> project flavoursInStock

    printStockOfFlavour Vanilla stock

    0 // return an integer exit code