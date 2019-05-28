module Main exposing (Msg(..), Pokemon, init, main, pokeDecoder, pokeDiv, pokeListDecoder, pokeListDiv, pokeString, subscriptions, update, view)

import Browser
import Html exposing (Html, b, br, div, h2, img, input, pre, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onMouseOut, onMouseOver)
import Http
import Json.Decode as JD exposing (Decoder, field, float, int, list, string)
import Json.Decode.Pipeline exposing (custom, hardcoded, optional, required)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { pokeLoadStatus : PokeLoadMsg
    , pokeSearchString : String
    , selectedPokeNum : String
    , selectedPokeImg : String
    , mouseOverPokeNum : String
    , pokeFullLoadStatus : PokeFullLoadMsg
    }


type PokeLoadMsg
    = Failure Http.Error
    | Loading
    | Success (List Pokemon)


type PokeFullLoadMsg
    = FailureFull Http.Error
    | LoadingFull
    | SuccessFull PokeFull


type alias Pokemon =
    { num : String
    , name : String
    , img : String
    , sta : String
    , atk : String
    , def : String
    , evo : String
    , typ : String
    , catch_rate : String
    , flee_rate : String
    , prim_atks : String
    , secu_atks : String
    }


type alias PokeFull =
    { id : Int
    , name : String
    , type1 : String
    , type2 : String
    , atk : Int
    , sta : Int
    , def : Int
    , isMyth : Int
    , isLegen : Int
    , gen : Int
    , candy : Int
    , kms : Int
    , desc : String
    , weight : Float
    , height : Float
    , weather : List String
    , cpList : PokeFullCPList
    }


type alias PokeFullCPList =
    { max : Int
    , wildMax : Int
    , wildMin : Int
    , weatherMax : Int
    , weatherMin : Int
    , eggMax : Int
    , eggMin : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { pokeLoadStatus = Loading
      , pokeSearchString = ""
      , selectedPokeNum = ""
      , selectedPokeImg = ""
      , mouseOverPokeNum = ""
      , pokeFullLoadStatus = LoadingFull
      }
    , Http.get
        { url = pokeProxy ++ "https://gamepress.gg/sites/default/files/aggregatedjson/pokemon-data-full-en-PoGO.json"
        , expect = Http.expectJson GotPokeList pokeListDecoder
        }
    )



-- DECODER


pokeFullDecoder : Decoder PokeFull
pokeFullDecoder =
    JD.succeed PokeFull
        |> optional "id" int -1
        |> optional "name" string ""
        |> optional "type1" string ""
        |> optional "type2" string ""
        |> optional "atk" int -1
        |> optional "sta" int -1
        |> optional "def" int -1
        |> optional "isMythical" int -1
        |> optional "isLegendary" int -1
        |> optional "generation" int -1
        |> optional "candyToEvolve" int -1
        |> optional "kmBuddyDistance" int -1
        |> optional "description" string ""
        |> optional "weight" float -1.0
        |> optional "height" float -1.0
        |> optional "weatherInfluences" (list string) []
        |> custom (JD.field "CPs" pokeFullCPListDecoder)


pokeFullCPListDecoder : Decoder PokeFullCPList
pokeFullCPListDecoder =
    JD.map7 PokeFullCPList
        (field "max" int)
        (field "wildMax" int)
        (field "wildMin" int)
        (field "weatherMax" int)
        (field "weatherMin" int)
        (field "eggMax" int)
        (field "eggMin" int)


pokeListDecoder : Decoder (List Pokemon)
pokeListDecoder =
    JD.list pokeDecoder


pokeDecoder : Decoder Pokemon
pokeDecoder =
    JD.succeed Pokemon
        |> required "number" string
        |> required "title_1" string
        |> required "uri" string
        |> required "sta" string
        |> required "atk" string
        |> required "def" string
        |> required "field_evolutions" string
        |> required "field_pokemon_type" string
        |> required "catch_rate" string
        |> required "field_flee_rate" string
        |> required "field_primary_moves" string
        |> required "field_secondary_moves" string



-- UPDATE


type Msg
    = GotPokeList (Result Http.Error (List Pokemon))
    | PokeSearch String
    | PokeDivSelected Pokemon
    | PokeDivMouseOver String
    | PokeDivMouseOut String
    | GotPokeFull (Result Http.Error PokeFull)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPokeList result ->
            case result of
                Ok pokesList ->
                    ( { model | pokeLoadStatus = Success pokesList }, Cmd.none )

                Err error ->
                    ( { model | pokeLoadStatus = Failure error }, Cmd.none )

        PokeSearch searchStr ->
            ( { model | pokeSearchString = String.toLower searchStr }, Cmd.none )

        PokeDivMouseOver pokeNum ->
            ( { model | mouseOverPokeNum = pokeNum }, Cmd.none )

        PokeDivMouseOut pokeNum ->
            ( { model | mouseOverPokeNum = "" }, Cmd.none )

        PokeDivSelected poke ->
            let
                newPokeNum =
                    model.selectedPokeNum /= poke.num
            in
            ( { model
                | selectedPokeNum =
                    if newPokeNum then
                        poke.num

                    else
                        ""
                , selectedPokeImg = poke.img
                , pokeFullLoadStatus =
                    if newPokeNum then
                        LoadingFull

                    else
                        model.pokeFullLoadStatus
              }
            , if newPokeNum then
                Http.get
                    { url = pokeProxy ++ "https://db.pokemongohub.net/api/pokemon2/" ++ poke.num ++ "/"
                    , expect = Http.expectJson GotPokeFull pokeFullDecoder
                    }

              else
                Cmd.none
            )

        GotPokeFull result ->
            case result of
                Ok pokeFull ->
                    ( { model | pokeFullLoadStatus = SuccessFull pokeFull }, Cmd.none )

                Err error ->
                    ( { model | pokeFullLoadStatus = FailureFull error }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ case model.pokeLoadStatus of
            Failure error ->
                statusViewError "Loading Pokemons Failed" error

            Loading ->
                statusViewLoading "Loading Pokemons"

            Success pokesList ->
                div [ class "container bg-light" ]
                    [ div
                        [ class "sticky-top bg-light pb-2 pt-2"
                        ]
                        [ div
                            [ style "border" "1px solid black"
                            , style "padding" "4px"
                            ]
                            [ input
                                [ class "bg-light"
                                , placeholder "search"
                                , style "border" "none"
                                , style "width" "100%"
                                , style "outline" "none"
                                , onInput PokeSearch
                                , title "You can filter your search with:\nnum:, name:, sta:, atk:, def:, evo:, type:"
                                ]
                                []
                            ]
                        , div
                            [ style "margin-top" "8px"
                            , style "border" "1px solid black"
                            , style "display"
                                (if String.isEmpty model.selectedPokeNum then
                                    "none"

                                 else
                                    ""
                                )
                            ]
                            [ pokeFullView model ]
                        ]
                    , div
                        [ style "border-top-style" "solid"
                        , style "border-top-color" "black"
                        , style "border-top-width" "1px"
                        , style "margin-top" "8px"
                        ]
                      <|
                        pokeListDiv pokesList model
                    ]
        ]


pokeFullView : Model -> Html Msg
pokeFullView model =
    case model.pokeFullLoadStatus of
        FailureFull error ->
            statusViewError "Loading Pokemon Info Failed" error

        LoadingFull ->
            statusViewLoading "Loading Pokemon"

        SuccessFull pokeFull ->
            div []
                [ div [ style "display" "grid" ] [ img [ width 200, style "margin" "auto", src model.selectedPokeImg ] [] ]
                , div [ style "text-align" "center" ] [ h2 [ style "margin" "0px" ] [ text pokeFull.name ] ]
                , div [ style "padding" "8px 4px" ] [ text pokeFull.desc ]
                ]


statusViewLoading : String -> Html Msg
statusViewLoading displayText =
    div []
        [ div [ class "text-center mt-2" ] [ text displayText ]
        , div [ class "text-center m-4" ]
            [ div [ class "spinner-border text-dark" ] []
            ]
        ]


statusViewError : String -> Http.Error -> Html Msg
statusViewError displayText error =
    div [ class "container" ]
        [ div [ class "row text-center" ]
            [ div [ class "col-12 py-3" ] [ text displayText ]
            , div [ class "col-12 py-2" ] [ img [ src "https://vignette.wikia.nocookie.net/es.pokemon/images/4/4c/Kabuto_NB.gif" ] [] ]
            , div [ class "col-12 pt-3" ] [ pre [ class "text-danger text-truncate" ] [ text (String.left 143 (Debug.toString error)) ] ]
            ]
        ]


pokeDiv : Pokemon -> Model -> Html Msg
pokeDiv poke model =
    div
        [ onClick (PokeDivSelected poke)
        , onMouseOver (PokeDivMouseOver poke.num)
        , onMouseOut (PokeDivMouseOut poke.num)
        , style "display" "flex"
        , style "border" "1px solid Silver"
        , style "border" "0px 0px"
        , style "display"
            (if String.contains model.pokeSearchString (pokeString poke) then
                "flex"

             else
                "none"
            )
        , style "border"
            (if model.mouseOverPokeNum == poke.num || poke.num == model.selectedPokeNum then
                "3px solid CornflowerBlue"

             else
                "1px solid Silver"
            )
        , style "background-color"
            (if poke.num == model.selectedPokeNum then
                "lightSteelBlue"

             else
                ""
            )
        ]
        [ div
            [ style "width" "70px"
            , style "display" "grid"
            ]
            [ img [ src poke.img, width 50, style "margin" "auto" ] []
            ]
        , div
            [ style "width" "78%"
            , style "padding" "4px"
            ]
            [ b [] [ text (poke.num ++ ": " ++ poke.name) ]
            , text (" [ " ++ poke.typ ++ " ]")
            , div []
                [ b [] [ text "Stats: " ]
                , text ("atk:" ++ poke.atk ++ " | def:" ++ poke.def ++ " | sta:" ++ poke.sta)
                ]
            , div []
                [ b [] [ text "Evolutions: " ]
                , if String.isEmpty poke.evo then
                    text "No evolutions"

                  else
                    text poke.evo
                ]
            , div [ style "display" "none" ]
                [ b [] [ text "Attacks:" ]
                , div [] [ text ("- " ++ poke.prim_atks) ]
                , div [] [ text ("- " ++ poke.secu_atks) ]
                ]
            ]
        ]


pokeListDiv : List Pokemon -> Model -> List (Html Msg)
pokeListDiv pokes model =
    List.map (\poke -> pokeDiv poke model) pokes



-- HELPERS


pokeString : Pokemon -> String
pokeString poke =
    String.toLower
        ("num:"
            ++ poke.num
            ++ "name:"
            ++ poke.name
            ++ "sta:"
            ++ poke.sta
            ++ "atk:"
            ++ poke.atk
            ++ "def:"
            ++ poke.def
            ++ pokeStringEvos poke.evo
            ++ pokeStringTypes poke.typ
         -- ++ poke.prim_atks
         -- ++ poke.secu_atks
        )


pokeStringTypes : String -> String
pokeStringTypes types =
    String.join " " (List.map (\typ -> "type:" ++ typ) (String.split "," (String.replace " " "" types)))


pokeStringEvos : String -> String
pokeStringEvos evos =
    String.join " " (List.map (\evo -> "evo:" ++ evo) (String.split "," (String.replace " " "" evos)))


pokeProxy : String
pokeProxy =
    "https://api.codetabs.com/v1/proxy?quest="
