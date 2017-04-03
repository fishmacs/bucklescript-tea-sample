open Tea
open Html

module Progress = Http.Progress

type model = {
  progress: Progress.t;
  bookUrl: string option;
  bookContent: string
}

let initModel = {
  progress = Progress.emptyProgress;
  bookUrl = None;
  bookContent = ""
}

let init () = initModel, Cmd.none

type msg =
    NoOp
  | GetBook of string
  | GetBookProgress of string * Progress.t
  | GetBookDone of string * string

let subscriptions _ = Sub.none

let progressHelper b = let open Progress in {
    bytes = b; bytesExpected = 1
  }

let update model = function
    NoOp -> model, Cmd.none
  | GetBook url ->
    let httpCmd =
      Http.getString url
      |> Http.Progress.track (fun progress -> GetBookProgress (url, progress))
      |> Http.send
        ( function
            Result.Error _ -> NoOp
          | Result.Ok output -> GetBookDone (url, output)
        )
    in {
      model with
      bookUrl = Some url;
      progress = progressHelper 0
    }, httpCmd
  | GetBookProgress (url, progress) ->
    if (Some url) <> model.bookUrl then model, Cmd.none else
      { model with progress}, Cmd.none
  | GetBookDone (url, bookContent) ->
    if (Some url) <> model.bookUrl then model, Cmd.none
    else { model with bookContent; progress = progressHelper 1 }, Cmd.none

let viewStyle = styles [
    ("display", "flex");
    ("flex-direction", "column");
    ("width", "100%");
    ("margin", "0 auto");
    ("font-family", "Arial")
  ]

let bookTextViewStyle = styles [
    ("height", "400px");
    ("width", "100%")
  ]

let inputRadio labelText url = div [] [
    label
      [ onCheck ( function
            | true -> GetBook url
            | _ -> NoOp )
      ]
      [ input' [ type' "radio"; name "book-radio" ] [];
        text labelText ]
  ]

let progressView loaded = div [] [
    span [] [ text "Progress: " ]
  ; progress
      [ value loaded;
        Attributes.max "100" ]
      [ text (loaded ^ "%") ]
  ; text (loaded ^ "%")
  ]

let progressLoaded progress = let open Progress in
  let bytes = float_of_int progress.bytes in
  let bytesExpected = float_of_int progress.bytesExpected in
  if bytesExpected <= 0.0 then 100
  else int_of_float (100. *. bytes /. bytesExpected)

let footerView = span [] [
    text "Books from "
  ; a [ href "http://www.gutenberg.org/";
        target "_blank" ]
      [ text "Project Gutenberg"]
  ]

let bookTextView valueText = div [] [
    textarea [
      value valueText;
      Attributes.disabled true;
      bookTextViewStyle
    ] []
  ]

let view model = div [viewStyle] [
    h1 [] [ text "Book Reader" ]
  ; p []
      [ text "Select a book:"
      ; inputRadio
          "Essays - Ralph Waldo Emerson"
          "https://s3-sa-east-1.amazonaws.com/estadistas/Essays-Ralph-Waldo-Emerson.txt"
      ; inputRadio
          "Leviathan - thomas Hobbes"
          "https://s3-sa-east-1.amazonaws.com/estadistas/Leviathan.txt"
      ; inputRadio
          "The Ethics of Aristotle - Aristotle"
          "https://s3-sa-east-1.amazonaws.com/estadistas/The-Ethics-of+Aristotle.txt"
      ]
  ; progressView (string_of_int (progressLoaded model.progress))
  ; bookTextView model.bookContent
  ; footerView
  ]

let main = App.standardProgram {
    init;
    update;
    view;
    subscriptions
  }
