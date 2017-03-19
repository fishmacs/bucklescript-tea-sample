open Tea
open App
open Html

type msg =
    Counters of CounterParts.msg
  | AddCounter
  | RemoveCounter

type model = {
  counters: msg CounterParts.model;
  count: int
}

let init () = {
  counters = CounterParts.init 4 (fun sm -> Counters sm);
  count = 0
}, Cmd.none

let update model = function
    Counters cMsg -> let () = Js.log (model, cMsg) in {
      model with
      counters = CounterParts.update model.counters cMsg
    }, Cmd.none
  | AddCounter -> {
      model with
      count = model.count + 1
    }, Cmd.none
  | RemoveCounter -> {
      model with
      count = model.count - 1
    }, CounterParts.shutdown model.counters model.count

let view model =
  let showCounter () =
    let rec showCounter' l a b =
      if a > b then l
      else showCounter' (CounterParts.view b model.counters :: l) a (b - 1)
    in showCounter' [] 1 model.count in
  div
    []
    [ button
        [ onClick AddCounter ]
        [ text "Append a Counter" ]
    ; if model.count = 0 then noNode
      else button
          [ onClick RemoveCounter ]
          [ text "Delete a Counter" ]
    ; div [] (showCounter ())
    ]

let main =
  standardProgram {
    init;
    update;
    view;
    subscriptions = fun _model -> Sub.none
  }
