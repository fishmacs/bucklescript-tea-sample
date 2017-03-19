open Tea.App
open Tea.Html

type msg =
    Counter of int * CounterBase.msg
  | AddCounter
  | RemoveCounter

type model = { counters: CounterBase.model list }

let update model = function
    Counter (idx, msg) -> let () = Js.log (model, idx, msg) in {
      counters = model.counters |>
                 List.mapi (fun i m -> if i <> idx then m else CounterBase.update m msg)
    }
  | AddCounter -> {
      counters = CounterBase.init () :: model.counters;
    }
  | RemoveCounter -> {
      counters = List.tl model.counters
    }

let view model =
  div
    []
    [ button
        [ onClick AddCounter ]
        [ text "Prepend a Counter" ]
    ; if List.length model.counters = 0 then noNode
      else button
          [ onClick RemoveCounter ]
          [ text "Delete a Counter" ]
    ; div
        []
        (List.mapi
           (fun i mo -> CounterBase.view (fun msg -> Counter (i, msg)) mo)
           model.counters)
    ]

let main = beginnerProgram {
    model = { counters = [] };
    update;
    view
  }
