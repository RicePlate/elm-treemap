module Tree exposing (..)

import String
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes as Attr
import DynamicStyle exposing (hover')
import Html.App

import Debug exposing (log)

type Msg = MouseClick String

type alias Area = Float
{-| Keep a list of areas sorted greatest to least. -}
type alias Areas = List Area
{-| The bounding area of the visualization. -}
type alias BoundingBox  = { x : Float, y : Float, width : Float, height: Float}

{-| Each rectangle is parametrized by its top-left corner and its bottom-right corner. -}
type alias Rectangle = {x1 : Float, y1 : Float, x2 : Float, y2 : Float}

{-| Configuration for your treemap.
  width/height: Defines the bounding area for the visualization.
  text: A list of strings, one per square in the visualization.
  color: A list of strings, one color per square in the visualization.
-}
type alias TreemapConfig = { width : Int, height : Int, text : List String, color : List String }

{-| Pass messages back to your application.
    For example, do some custom thing for onclick events
-}
type alias UpdateConfig msg = { onclick : String -> msg }


newbox : BoundingBox -> Area -> BoundingBox
newbox oldbox covered =
  if oldbox.width >= oldbox.height then
    let area = covered / oldbox.height
        width = oldbox.width - area
    in
    -- shift the top-left corner to the right an amount "area"
    -- so that it is at the edge of the covered area.
    -- Also remove the covered area "width" from the remaining width.
    { x = oldbox.x + area, y = oldbox.y, width = width, height = oldbox.height}
  else
    let area = covered / oldbox.width
        height = oldbox.height - area
    in
    -- shift the top-left corner down by an amount "area"
    -- so that it is at the edge of the covered area.
    -- Also remove the covered area "height" from the remaining height.
    { x = oldbox.x, y = oldbox.y + area, width = oldbox.width, height = height}

layout : Areas -> BoundingBox -> List Rectangle
layout squareas bb =
  let
    width = List.sum squareas / bb.height
    height = List.sum squareas / bb.width
    xoffset = bb.x
    yoffset = bb.y
    yoffsets = List.scanl (\next prev -> prev + next/width)  yoffset squareas
    xoffsets = List.scanl (\next prev -> prev + next/height) xoffset squareas
  in
  if bb.width >= bb.height then
    List.map2 (\r offset -> { x1 = bb.x, y1 = offset, x2 = bb.x + width, y2 = offset + r / width}) squareas yoffsets
  else
    List.map2 (\r offset -> { x1 = offset, y1 = bb.y, x2 = offset + r / height, y2 = bb.y + height}) squareas xoffsets

worst : Areas -> Float -> Float
worst squareas l2 =
  let mm = minmax squareas
      s = List.sum squareas
      s2= s*s
  in
    Basics.max ((l2*(snd mm)) / s2) (s2 / (l2*(fst mm)))

minmax : Areas -> (Float, Float)
minmax squareas = (List.foldl min (1/0) squareas, List.foldl max 0 squareas )

squarify_helper : Areas -> Areas -> BoundingBox -> List Rectangle
squarify_helper areas squareas box =
  let
    t = case List.tail areas of
        Just x -> x
        Nothing -> []
    c = case List.head areas of
        Just x -> x
        Nothing -> -1.0
  in
    if c<0 then layout squareas box
    else
      let
        squareasp1 = List.append squareas [c]
        len = min box.height box.width
        l2 = len*len
      in
      case List.length squareas of
      0 -> squarify_helper t squareasp1 box
      _ ->
        if (worst squareas l2) > (worst squareasp1 l2) then  -- paper has typo on inequality here
          squarify_helper t squareasp1 box
        else
          let bb = newbox box (List.sum squareas)
              sofar = layout squareas box
          in
          sofar ++ (squarify areas bb)

-- take the areas of each square and the large bounding
-- rectangle that will contain the treemap
squarify : Areas -> BoundingBox -> List Rectangle
squarify values box = squarify_helper values [] box

norm : Area -> Areas -> Areas
norm area areas =
  let total = List.sum areas
      multiplier = area / total
  in
    List.map (\v -> v * multiplier) areas

fsize : Float -> Float -> Float -> Float
fsize avg w h =
  let r = sqrt (w * h) in
  min (r / avg) 20.0  -- max font size of 20

px : Float -> String
px v = (toString v) ++ "px"

avglen : List String -> Float
avglen textTags =
  let s = Basics.toFloat <| List.sum <| List.map String.length textTags
      n = Basics.toFloat <| List.length textTags
  in
    s / n

update : UpdateConfig msg -> Msg -> msg
update config msg =
  case msg of
    MouseClick id -> config.onclick id

treemap : Areas -> TreemapConfig -> List (Html Msg)
treemap areas config =
  let
    w = Basics.toFloat config.width
    h = Basics.toFloat config.height
    a = w * h
    normarea = norm a areas
    avg = avglen config.text
    squared = squarify normarea { x = 0, y = 0, width = w, height = h }
  in
    List.map3 (\coord text color -> f w h avg coord text color) squared config.text config.color

f : Float -> Float -> Float -> Rectangle -> String -> String -> Html Msg
f width height average coord text color =
  let
    w = coord.x2 - coord.x1
    h = coord.y2 - coord.y1
    left = coord.x1 + (w / 2) - (width / 2)
    top = (height / 2) - (h / 2) - coord.y1
    size = fsize average w h
  in
    div (List.append [onClick (MouseClick text)] (hover' [("width", px w),
                      ("height", px h),
                      ("left", px coord.x1),
                      ("top", px coord.y1),
                      ("background-color", color),
                      ("position", "absolute")]
                [("opacity","1","0.4"),
                 ("border", "1px solid black", "2px solid blue"),
                 ("box-shadow", "0px 0px 0px rgba(0,0,0,0)", "0px 0px 30px rgba(0,0,0,0.8)")]))  [Html.text text]