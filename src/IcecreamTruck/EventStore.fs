module EventStore

type EventProducer<'Event> = 
    'Event list -> 'Event list

type Aggregate = System.Guid

type EventStore<'Event> = {
    Get: unit -> Map<Aggregate, 'Event list>
    GetStream: Aggregate -> 'Event list
    Append: Aggregate -> 'Event list -> unit
    Evolve: Aggregate -> EventProducer<'Event> -> unit
} 

type Msg<'Event> = 
    | Append of Aggregate * 'Event list
    | Get of AsyncReplyChannel<Map<Aggregate, 'Event list>>
    | GetStream of Aggregate * AsyncReplyChannel<'Event list>
    | Evolve of Aggregate * EventProducer<'Event>

let eventsForAggregate aggregate history = 
    history
    |> Map.tryFind aggregate
    |> Option.defaultValue []

let initialize (): EventStore<'Event> = 
    let agent = 
        MailboxProcessor.Start(fun inbox -> 
            let rec loop history = 
                async {
                    let! msg = inbox.Receive()

                    match msg with
                    | Append (aggregate, events)  ->
                        let streamEvents =
                            history |> eventsForAggregate aggregate

                        let newHistory = 
                            history
                            |> Map.add aggregate (streamEvents @ events)

                        return! loop newHistory

                    | Get reply ->
                        reply.Reply history

                        return! loop history

                    | GetStream (aggregate, reply) ->
                        reply.Reply (history |> eventsForAggregate aggregate)

                        return! loop history

                    | Evolve (aggregate, eventProducer) ->
                        let streamEvents = 
                            history 
                            |> eventsForAggregate aggregate

                        let newEvents = eventProducer streamEvents
                        let newHistory =
                            history 
                            |> Map.add aggregate (streamEvents @ newEvents)

                        return! loop newHistory
                }

            loop Map.empty
        )

    let append aggregate events = 
        agent.Post (Append (aggregate, events))

    let get () = 
        agent.PostAndReply Get

    let getStream aggregate =
        agent.PostAndReply (fun reply -> GetStream (aggregate, reply))

    let evolve aggregate eventProducer = 
        agent.Post (Evolve (aggregate, eventProducer))

    {
        Append = append
        Get = get
        GetStream = getStream
        Evolve = evolve
    }