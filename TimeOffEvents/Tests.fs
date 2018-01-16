module TimeOff.Tests

open Expecto
open EventStorage

let Given events = events
let When command events = events, command
let Then expected message (events: RequestEvent list, command) =
    let store = InMemoryStore.Create<UserId, RequestEvent>()
    for event in events do
      let stream = store.GetStream event.Request.UserId
      stream.Append [event]
    let result = Logic.handleCommand store command
    Expect.equal result expected message

open System

// Tests on request creation
let creationTests =
  testList "Employee creates request tests" [
    test "A request is created" {
      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = DateTime(2018, 12, 30); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 30); HalfDay = PM } }

      Given [ ]
      |> When (RequestTimeOff request)
      |> Then (Ok [RequestCreated request]) "The request should be created"
    }
  ]

// Tests on request validation
let validationTests =
  testList "Manager validates request tests" [
    
    test "A pending request is validated" {
      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = DateTime(2017, 12, 30); HalfDay = AM }
        End = { Date = DateTime(2017, 12, 30); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> When (ValidateRequest (1, Guid.Empty))
      |> Then (Ok [RequestValidated request]) "The request should be validated"
    }
    
    test "A validated request cannot be validated" {
      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = DateTime(2017, 12, 30); HalfDay = AM }
        End = { Date = DateTime(2017, 12, 30); HalfDay = PM } }

      Given [ RequestValidated request ]
      |> When (ValidateRequest (1, Guid.Empty))
      |> Then (Error "The request cannot be validated") "The request should not be validated"
    }
    test "A refused request cannot be validated" {
      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = DateTime(2017, 12, 30); HalfDay = AM }
        End = { Date = DateTime(2017, 12, 30); HalfDay = PM } }

      Given [ RequestRefused request ]
      |> When (ValidateRequest (1, Guid.Empty))
      |> Then (Error "The request cannot be validated") "The request should not be validated"
    }
    test "A request asked for cancellation cannot be validated" {
      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = DateTime(2017, 12, 30); HalfDay = AM }
        End = { Date = DateTime(2017, 12, 30); HalfDay = PM } }

      Given [ RequestAskedForCancellation request ]
      |> When (ValidateRequest (1, Guid.Empty))
      |> Then (Error "The request cannot be validated") "The request should not be validated"
    }
    test "A request canceled by employee cannot be validated" {
      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = DateTime(2017, 12, 30); HalfDay = AM }
        End = { Date = DateTime(2017, 12, 30); HalfDay = PM } }

      Given [ RequestCanceledByEmployee request ]
      |> When (ValidateRequest (1, Guid.Empty))
      |> Then (Error "The request cannot be validated") "The request should not be validated"
    }
  ]

// Tests on request refusing
let refuseTests =
  testList "Manager refuses request tests" [
    
    test "A pending request can be refused" {
      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = DateTime(2017, 12, 30); HalfDay = AM }
        End = { Date = DateTime(2017, 12, 30); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> When (RefuseRequest (1, Guid.Empty))
      |> Then (Ok [RequestRefused request]) "The request should be refused"
    }
    
    test "A validated request cannot be refused" {
      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = DateTime(2017, 12, 30); HalfDay = AM }
        End = { Date = DateTime(2017, 12, 30); HalfDay = PM } }

      Given [ RequestValidated request ]
      |> When (RefuseRequest (1, Guid.Empty))
      |> Then (Error "The request cannot be refused") "The request should not be refused"
    }
    test "A refused request cannot be refused" {
      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = DateTime(2017, 12, 30); HalfDay = AM }
        End = { Date = DateTime(2017, 12, 30); HalfDay = PM } }

      Given [ RequestRefused request ]
      |> When (RefuseRequest (1, Guid.Empty))
      |> Then (Error "The request cannot be refused") "The request should not be refused"
    }
    test "A request asked for cancellation cannot be refused" {
      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = DateTime(2017, 12, 30); HalfDay = AM }
        End = { Date = DateTime(2017, 12, 30); HalfDay = PM } }

      Given [ RequestAskedForCancellation request ]
      |> When (RefuseRequest (1, Guid.Empty))
      |> Then (Error "The request cannot be refused") "The request should not be refused"
    }
    test "A request canceled by manager cannot be refused" {
      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = DateTime(2017, 12, 30); HalfDay = AM }
        End = { Date = DateTime(2017, 12, 30); HalfDay = PM } }

      Given [ RequestCanceledByManager request ]
      |> When (RefuseRequest (1, Guid.Empty))
      |> Then (Error "The request cannot be refused") "The request should not be refused"
    }
  ]


let askForCancellationTests =
  testList "Employee asks for cancellation tests" [
    
    test "A passed pending request can be asked for cancellation by employee" {
      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = DateTime(2017, 12, 30); HalfDay = AM }
        End = { Date = DateTime(2017, 12, 30); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> When (AskForCancellation (1, Guid.Empty))
      |> Then (Ok [RequestAskedForCancellation request]) "The request should be asked for cancellation"
    }
    test "A passed validated request can be asked for cancellation by employee" {
      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = DateTime(2017, 12, 30); HalfDay = AM }
        End = { Date = DateTime(2017, 12, 30); HalfDay = PM } }

      Given [ RequestValidated request ]
      |> When (AskForCancellation (1, Guid.Empty))
      |> Then (Ok [RequestAskedForCancellation request]) "The request should be asked for cancellation"
    }
    
    test "A future pending request cannot be asked for cancellation by employee" {
      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = DateTime(2018, 12, 30); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 30); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> When (AskForCancellation (1, Guid.Empty))
      |> Then (Error "The request cannot be asked for cancellation") "The request should not be asked for cancellation"
    }
    test "A passed refused request cannot be asked for cancellation by employee" {
      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = DateTime(2017, 12, 30); HalfDay = AM }
        End = { Date = DateTime(2017, 12, 30); HalfDay = PM } }

      Given [ RequestRefused request ]
      |> When (AskForCancellation (1, Guid.Empty))
      |> Then (Error "The request cannot be asked for cancellation") "The request should not be asked for cancellation"
    }
  ]


let refuseRequestCancellationTests =
  testList "Manager refuses request cancellation tests" [
    
    test "A request asked for cancellation can be refused by manager" {
      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = DateTime(2017, 12, 30); HalfDay = AM }
        End = { Date = DateTime(2017, 12, 30); HalfDay = PM } }

      Given [ RequestAskedForCancellation request ]
      |> When (RefuseCancellation (1, Guid.Empty))
      |> Then (Ok [RequestCancellationRefused request]) "The request asked for cancellation should be refused"
    }

    test "A request canceled by manager cannot be refused by manager" {
      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = DateTime(2017, 12, 30); HalfDay = AM }
        End = { Date = DateTime(2017, 12, 30); HalfDay = PM } }

      Given [ RequestCanceledByManager request ]
      |> When (RefuseCancellation (1, Guid.Empty))
      |> Then (Error "The request cancellation cannot be refused") "The request asked for cancellation should not be refused"
    }
  ]


let cancelRequestByEmployeeTests =
  testList "Employee cancels request tests" [
    
    test "A future pending request can be canceled by employee" {
      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = DateTime(2018, 12, 30); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 30); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> When (CancelByEmployee (1, Guid.Empty))
      |> Then (Ok [RequestCanceledByEmployee request]) "The request should be canceled by employee"
    }
    test "A future validated request can be canceled by employee" {
      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = DateTime(2018, 12, 30); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 30); HalfDay = PM } }

      Given [ RequestValidated request ]
      |> When (CancelByEmployee (1, Guid.Empty))
      |> Then (Ok [RequestCanceledByEmployee request]) "The request should be canceled by employee"
    }
    
    test "A passed pending request cannot be canceled by employee" {
      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = DateTime(2017, 12, 30); HalfDay = AM }
        End = { Date = DateTime(2017, 12, 30); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> When (CancelByEmployee (1, Guid.Empty))
      |> Then (Error "The request cannot be canceled by the employee") "The request should not be canceled by employee"
    }
    test "A future refused request cannot be canceled by employee" {
      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = DateTime(2018, 12, 30); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 30); HalfDay = PM } }

      Given [ RequestRefused request ]
      |> When (CancelByEmployee (1, Guid.Empty))
      |> Then (Error "The request cannot be canceled by the employee") "The request should not be canceled by employee"
    }
  ]


let cancelRequestByManagerTests =
  testList "Manager cancels request tests" [
    
    test "A pending request can be canceled by manager" {
      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = DateTime(2018, 12, 30); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 30); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> When (CancelByManager (1, Guid.Empty))
      |> Then (Ok [RequestCanceledByManager request]) "The request should be canceled by manager"
    }
    test "A validated request can be canceled by manager" {
      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = DateTime(2018, 12, 30); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 30); HalfDay = PM } }

      Given [ RequestValidated request ]
      |> When (CancelByManager (1, Guid.Empty))
      |> Then (Ok [RequestCanceledByManager request]) "The request should be canceled by manager"
    }
    test "A request asked for cancellation can be canceled by manager" {
      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = DateTime(2018, 12, 30); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 30); HalfDay = PM } }

      Given [ RequestAskedForCancellation request ]
      |> When (CancelByManager (1, Guid.Empty))
      |> Then (Ok [RequestCanceledByManager request]) "The request should be canceled by manager"
    }
    test "A cancellation request refused can be canceled by manager" {
      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = DateTime(2018, 12, 30); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 30); HalfDay = PM } }

      Given [ RequestCancellationRefused request ]
      |> When (CancelByManager (1, Guid.Empty))
      |> Then (Ok [RequestCanceledByManager request]) "The request should be canceled by manager"
    }

    test "A refused request cannot be canceled by manager" {
      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = DateTime(2018, 12, 30); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 30); HalfDay = PM } }

      Given [ RequestRefused request ]
      |> When (CancelByManager (1, Guid.Empty))
      |> Then (Error "The request cannot be canceled by the manager") "The request should not be canceled by manager"
    }
    test "A request canceled by employee cannot be canceled by manager" {
      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = DateTime(2018, 12, 30); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 30); HalfDay = PM } }

      Given [ RequestCanceledByEmployee request ]
      |> When (CancelByManager (1, Guid.Empty))
      |> Then (Error "The request cannot be canceled by the manager") "The request should not be canceled by manager"
    }
    test "A request canceled by manager cannot be canceled by manager" {
      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = DateTime(2018, 12, 30); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 30); HalfDay = PM } }

      Given [ RequestCanceledByManager request ]
      |> When (CancelByManager (1, Guid.Empty))
      |> Then (Error "The request cannot be canceled by the manager") "The request should not be canceled by manager"
    }
  ]

let balanceTests =
  testList "Request balance of days off tests" [
    
    test "One future validated request in store" {
      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = DateTime(2018, 12, 30); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 30); HalfDay = PM } }

      let balance: Balance = {
        UserName = "userToDisplay"
        BalanceYear = 2018
        PortionAccruedToDate = 0.
        TakenToDate = 0.
        Planned = 1.
        CurrentBalance = -1.
      }

      Given [ RequestValidated request ]
      |> When (Balance 1)
      |> Then (Ok [RequestBalance balance]) "The request balance should be equals"
    }
  ]

let tests =
  testList "All tests" [
    creationTests
    validationTests
    refuseTests
    askForCancellationTests
    refuseRequestCancellationTests
    cancelRequestByEmployeeTests
    cancelRequestByManagerTests
    balanceTests
  ]