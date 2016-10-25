module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Html.App
import Tree exposing (..)
import Window exposing (..)
import Task

type alias Model = { w: Int, h: Int }
type Msg
  = Resize Window.Size
  | NoOp
  | TreeMsg Tree.Msg

cfg =
  { areas = [539.3438,
             537.3412,
             537.2178,
             536.321,
             530.23212,
             440.123,
             404.231,
             390.23,
             375.111,
             368.1,
             350.01],
    tags  = ["Square1",
             "Square2",
             "Square3",
             "Square4",
             "Square5",
             "Square6",
             "Square7",
             "Square8",
             "Square9",
             "Square10",
             "Square11"
             ],
    colors = ["#4286f4",
              "#6d668c",
              "#ff5664",
              "#88ed6d",
              "#166600",
              "#c6e820",
              "#e88e20",
              "#e85920",
              "#e8e420",
              "#20e8e1",
              "#ccd1ff"]
  }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp -> model ! []
    Resize s -> {model| w=s.width, h=s.height} ! []
    TreeMsg m -> model ! []

view : Model -> Html Msg 
view model = Html.App.map TreeMsg (div [] (treemap cfg.areas {width=model.w-200,height=model.h-200,text=cfg.tags,color=cfg.colors}))

subscriptions : Model -> Sub Msg 
subscriptions model = Window.resizes Resize

main : Program Never
main =
    Html.App.program
        { init = ({ w=0,h=0}, Task.perform (\_ -> NoOp) (\s -> Resize s) Window.size)
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
