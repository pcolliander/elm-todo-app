import Dom exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Ternary exposing (..)
import List exposing (..)

import Json.Decode as Json
import String
import Task


main =
  Html.beginnerProgram { model = model, view = view, update = update }

-- MODEL
type alias Model = { 
  todos : List Todo,
  inputValue : String,
  filterBy : Filter
}

type alias Todo = { id : Int, title : String, isCompleted : Bool}

type Filter = All | Completed | Remaining

model : Model
model = { todos = [], inputValue = "", filterBy = All}


-- UPDATE
type Msg = Create 
           | Change String 
           | Delete Int
           | ToggleComplete Int
           | ClearCompleted
           | KeyDown
           | ToggleCompleteAll
           | FilterBy Filter

update msg model =
  case msg of
    Create -> 
      { model | 
        todos = Todo (List.length model.todos + 1) model.inputValue False :: model.todos,
        inputValue = ""}

    Change newContent -> 
      { model | inputValue = newContent } 

    Delete id ->
      { model | todos = filter (\todo -> todo.id /= id) model.todos }

    ToggleComplete id -> 
      let 
        updateEntry t = (t.id == id) 
          ?  { t | isCompleted = not t.isCompleted } 
          <| t
      in 
        { model | todos = List.map updateEntry model.todos }

    ClearCompleted ->
      { model | todos = filter (\t -> t.isCompleted /= True) model.todos }

    KeyDown ->
      { model |
        todos = Todo (List.length model.todos + 1) model.inputValue False :: model.todos,
        inputValue = ""}

    ToggleCompleteAll ->
      { model |
          todos = List.map (\t -> { t | isCompleted = not t.isCompleted }) model.todos }

    FilterBy filterBy ->
      { model |
          filterBy = filterBy }

onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not ENTER"
    in
        on "keydown" (Json.andThen isEnter keyCode)

filterTodos : Filter -> Todo -> Bool
filterTodos filterBy todo =
  case filterBy of 
    Remaining ->
      not todo.isCompleted
    Completed ->
      todo.isCompleted
    All ->
      True

-- VIEW
(=>) = (,)

view model =
  div [style 
    [ "display" => "flex",
      "flex-direction" => "column",
      "width" => "250px",
      "margin" => "50px auto" ]]
    [ 
      button [ onClick ToggleCompleteAll] [ text "complete all"]
      , input [placeholder "Write something", onInput Change, onEnter KeyDown, value model.inputValue ] [] 
      , model.todos
        |> sortBy .id
        |> filter (filterTodos model.filterBy)

        |> List.map (\todo -> div [ style [ "display" => "flex", "justify-content" => "space-between" ]] [ li [ onClick (ToggleComplete todo.id), style [ "user-select" => "none", "text-decoration" => (todo.isCompleted ? "line-through" <| "")]] [ text todo.title ], span [ style [ "cursor" => "pointer"], onClick (Delete todo.id)] [ text "X" ]] )
        |> ul []

     , div [ style [ "display" => "flex", "justify-content" => "space-between"] ] 
       [ 
           span [ style [ "cursor" => "pointer" ], onClick (FilterBy All)] [ text "all"] 
         , span [ style [ "cursor" => "pointer" ], onClick (FilterBy Completed)] [ text "completed"] 
         , span [ style [ "cursor" => "pointer" ], onClick (FilterBy Remaining)] [ text "remaining"]]

     , button [ onClick Create ] [ text "Create" ]
     , button [ onClick ClearCompleted ] [ text "Clear completed"]
     
     , span [] [ ("Total number of todos: " ++ ((length model.todos) |> toString))  |> text ]
     , span [] [ ("Finished todos: " ++ (filter (\t -> t.isCompleted) model.todos |> length |> toString)) |> text ]
   ]

