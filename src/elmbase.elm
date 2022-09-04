import Browser
import Html exposing (Html, button, div, text)
main =  Browser.sandbox { init = init, update = update, view = view }
type alias Model = Int
type Msg = Reset |
update : Msg -> Model -> Model
update msg model =
  case msg of
view : Model -> Html Msg
view model =
  div []