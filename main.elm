import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Ternary exposing (..)
import List exposing (..)

main =
  Html.beginnerProgram { model = model, view = view, update = update }

-- MODEL
type alias Model = 
  { todos : List Todo,
    inputValue : String }

type alias Todo = { id : Int, title : String, isCompleted : Bool}

model : Model
model = 
  { todos = [],
    inputValue = ""}

(=>) = (,)

-- UPDATE
type Msg 
  = Create 
  | Change String 
  | Delete Int
  | ToggleComplete Int

update msg model =
  case msg of
    Create -> 
      { model | 
          todos = Todo (List.length model.todos + 1) model.inputValue False :: model.todos }

    Change newContent -> 
      { model | inputValue = newContent } 

    Delete id ->
      { model | 
          todos = filter (\todo -> todo.id /= id) model.todos }

    ToggleComplete id -> 
      let 
        updateEntry t = (t.id == id) 
          ?  { t | isCompleted = not t.isCompleted } 
          <| t
      in 
        { model | todos = List.map updateEntry model.todos }

      -- ALSO WORKING (My Own solution)
      -- let 
      --   todo = 
      --     model.todos 
      --     |> filter (\todo -> todo.id == id)
      --     |> List.map (\todo -> {todo | isCompleted = not todo.isCompleted } )
      --     |> head
      --
      -- in
      --   case todo of
      --     Just value ->
      --       { model |
      --         todos = 
      --           value :: (filter (\t -> t.id /= id) model.todos)}
      --     Nothing ->
      --       model

-- VIEW
view model =
  div [style 
    [ "display" => "flex",
      "flex-direction" => "column",
      "width" => "250px",
      "margin" => "50px auto" ]]

    [ input [placeholder "Write something", onInput Change ] [], 

      model.todos
        |> sortBy .id
        |> List.map (\todo -> div [ style [ "display" => "flex", "justify-content" => "space-between" ]] [ li [ onClick (ToggleComplete todo.id), style [ "text-decoration" => (todo.isCompleted ? "line-through" <| "")]] [ text todo.title ], span [ style [ "cursor" => "pointer"], onClick (Delete todo.id)] [ text "X" ]] )
        |> ul []

     , button [ onClick Create ] [ text "Create" ] 
    ]




