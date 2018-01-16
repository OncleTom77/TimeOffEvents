namespace TimeOff

open System
open EventStorage
open System.Reflection

type Balance = {
    UserName: string
    BalanceYear: int
    PortionAccruedToDate: float
    TakenToDate: float
    Planned: float
    CurrentBalance: float
}

type User =
    | Employee of int
    | Manager

type HalfDay = | AM | PM

type Boundary = {
    Date: DateTime
    HalfDay: HalfDay
}

type UserId = int

type TimeOffRequest = {
    UserId: UserId
    RequestId: Guid
    Start: Boundary
    End: Boundary
}

type Command =
    | RequestTimeOff of TimeOffRequest
    | ValidateRequest of UserId * Guid
    | RefuseRequest of UserId * Guid
    | AskForCancellation of UserId * Guid
    | RefuseCancellation of UserId * Guid
    | CancelByEmployee of UserId * Guid
    | CancelByManager of UserId * Guid
    | Balance of UserId
    with
    member this.UserId =
        match this with
        | RequestTimeOff request -> request.UserId
        | ValidateRequest (userId, _) -> userId
        | RefuseRequest (userId, _) -> userId
        | AskForCancellation (userId, _) -> userId
        | RefuseCancellation (userId, _) -> userId
        | CancelByEmployee (userId, _) -> userId
        | CancelByManager (userId, _) -> userId
        | Balance (userId) -> userId

type RequestEvent =
    | RequestCreated of TimeOffRequest
    | RequestValidated of TimeOffRequest
    | RequestRefused of TimeOffRequest
    | RequestAskedForCancellation of TimeOffRequest
    | RequestCancellationRefused of TimeOffRequest
    | RequestCanceledByEmployee of TimeOffRequest
    | RequestCanceledByManager of TimeOffRequest
    | RequestBalance of Balance
    with
    member this.Request =
        match this with
        | RequestCreated request -> request
        | RequestValidated request -> request
        | RequestRefused request -> request
        | RequestAskedForCancellation request -> request
        | RequestCancellationRefused request -> request
        | RequestCanceledByEmployee request -> request
        | RequestCanceledByManager request -> request
        | RequestBalance _ -> invalidOp "balance"
    member this.Balance =
        match this with
        | RequestBalance balance -> balance
        | _ -> invalidOp "request"

module Logic =

    type RequestState =
        | NotCreated
        | PendingValidation of TimeOffRequest
        | Validated of TimeOffRequest
        | Refused
        | AskedForCancellation of TimeOffRequest
        | CancellationRefused of TimeOffRequest
        | CanceledByEmployee
        | CanceledByManager
        with
        member this.Request =
            match this with
            | NotCreated -> invalidOp "Not created"
            | PendingValidation request
            | Validated request -> request
            | Refused -> invalidOp "Refused"
            | AskedForCancellation request -> request
            | CancellationRefused request -> request
            | CanceledByEmployee -> invalidOp "Canceled by employee"
            | CanceledByManager -> invalidOp "Canceled by manager"
        member this.IsActive =
            match this with
            | NotCreated | Refused | CanceledByEmployee | CanceledByManager -> false
            | PendingValidation _
            | Validated _ | AskedForCancellation _ | CancellationRefused _ -> true

    let evolve _ event =
        match event with
        | RequestCreated request -> PendingValidation request
        | RequestValidated request -> Validated request
        | RequestRefused _ -> Refused
        | RequestAskedForCancellation request -> AskedForCancellation request
        | RequestCancellationRefused request -> CancellationRefused request
        | RequestCanceledByEmployee _ -> CanceledByEmployee
        | RequestCanceledByManager _ -> CanceledByManager
        | RequestBalance _ -> invalidOp "balance"

    let getRequestState events =
        events |> Seq.fold evolve NotCreated

    let getAllRequests events =
        let folder requests (event: RequestEvent) =
            let state = defaultArg (Map.tryFind event.Request.RequestId requests) NotCreated
            let newState = evolve state event
            requests.Add (event.Request.RequestId, newState)

        events |> Seq.fold folder Map.empty

    let overlapWithAnyRequest (previousRequests: TimeOffRequest seq) request =
        false //TODO

    // employee creates a request. It is pending until manager validates, refuses or cancels it
    let createRequest previousRequests request =
        if overlapWithAnyRequest previousRequests request then
            Error "Overlapping request"
        elif request.Start.Date <= DateTime.Today then
            Error "The request starts in the past"
        else
            Ok [RequestCreated request]

    // manager validates a pending request
    let validateRequest requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestValidated request]
        | _ -> Error "The request cannot be validated"

    // manager refuses a request
    let refuseRequest requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestRefused request]
        | _ -> Error "The request cannot be refused"

    // employee asks for cancellation of pending or validated request when it starts in the past or today
    let askForCancellation requestState =
        match requestState with
        | PendingValidation request | Validated request ->
            if request.Start.Date <= DateTime.Today then
                Ok [RequestAskedForCancellation request]
            else
                Error "The request cannot be asked for cancellation"
        | _ -> Error "The request cannot be asked for cancellation"

    // manager refuses a cancellation of validated request
    let refuseRequestCancellation requestState =
        match requestState with
        | AskedForCancellation request ->
            Ok [RequestCancellationRefused request]
        | _ -> Error "The request cancellation cannot be refused"

    // employee cancels his requests (pending or validated) if it starts in the future
    let cancelRequestByEmployee requestState =
        match requestState with
        | PendingValidation request | Validated request ->
            if request.Start.Date > DateTime.Today then
                Ok [RequestCanceledByEmployee request]
            else
                Error "The request cannot be canceled by the employee"
        | _ -> Error "The request cannot be canceled by the employee"

    // manager cancels a request in any state : pending, validated, asked for cancellation, refused to cancel
    let cancelRequestByManager requestState =
        match requestState with
        | PendingValidation request | Validated request | AskedForCancellation request | CancellationRefused request ->
            Ok [RequestCanceledByManager request]
        | _ -> Error "The request cannot be canceled by the manager"
    
    // Compute the number of day off of the request
    let computeTimeOff request =
        let time = request.End.Date - request.Start.Date
        let mutable result = time.TotalDays + 1.0
        if request.End.HalfDay.Equals AM then do
            result <- result - 0.5
        if request.Start.HalfDay.Equals PM then do
            result <- result - 0.5
        result

    // Get balance of day off
    let getBalance (store: IStore<UserId, RequestEvent>) userId =
        let stream = store.GetStream userId
        let events = stream.ReadAll()
        let userRequests = getAllRequests events
        let activeRequests =
            userRequests
            |> Map.toSeq
            |> Seq.map (fun (_, state) -> state)
            |> Seq.where (fun state -> state.IsActive)
            |> Seq.map (fun state -> state.Request)
        
        let accrued = (25. / 12.) * float (DateTime.Today.Month - 1)

        let taken =
            Seq.sumBy computeTimeOff (activeRequests
            |> Seq.where (fun (request) -> request.Start.Date <= DateTime.Today))
        
        let planned =
            Seq.sumBy computeTimeOff (activeRequests
            |> Seq.where (fun (request) -> request.Start.Date > DateTime.Today))

        {
            UserName = "userToDisplay"
            BalanceYear = DateTime.Today.Year
            PortionAccruedToDate = accrued
            TakenToDate = taken
            Planned = planned
            CurrentBalance = accrued - (taken + planned)
        }

    let handleCommand (store: IStore<UserId, RequestEvent>) (command: Command) =
        let userId = command.UserId
        let stream = store.GetStream userId
        let events = stream.ReadAll()
        let userRequests = getAllRequests events

        match command with
        | RequestTimeOff request ->
            let activeRequests =
                userRequests
                |> Map.toSeq
                |> Seq.map (fun (_, state) -> state)
                |> Seq.where (fun state -> state.IsActive)
                |> Seq.map (fun state -> state.Request)

            createRequest activeRequests request

        | ValidateRequest (_, requestId) ->
            let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
            validateRequest requestState
        
        | RefuseRequest (_, requestId) ->
            let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
            refuseRequest requestState
        
        | AskForCancellation (_, requestId) ->
            let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
            askForCancellation requestState

        | RefuseCancellation (_, requestId) ->
            let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
            refuseRequestCancellation requestState

        | CancelByEmployee (_, requestId) ->
            let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
            cancelRequestByEmployee requestState

        | CancelByManager (_, requestId) ->
            let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
            cancelRequestByManager requestState
        | Balance (userId) ->
            let balance = getBalance store userId
            Ok [RequestBalance balance]

