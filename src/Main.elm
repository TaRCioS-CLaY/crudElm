port module Main exposing (..)

import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (..)



-- Main


main : Program String Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type alias Model =
    { nome : String
    , idade : Int
    , listaPessoas : List Pessoa
    }


type alias Pessoa =
    { nome : String
    , idade : Int
    }


init : String -> ( Model, Cmd Msg )
init pList =
    ( Model "" 0 (getPessoas pList), Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- Update


type Msg
    = Nome String
    | Idade String
    | Salvar Model



-- | Lista String


port cadastrar : Pessoa -> Cmd msg


port pessoasCadastradas : (String -> msg) -> Sub msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Nome nome ->
            ( { model | nome = nome }, Cmd.none )

        Idade idade ->
            case String.toInt idade of
                Just idadeCerta ->
                    ( { model | idade = idadeCerta }, Cmd.none )

                Nothing ->
                    ( { model | idade = verificarStringInt idade }, Cmd.none )

        Salvar _ ->
            ( { model | listaPessoas = Pessoa model.nome model.idade :: model.listaPessoas }, cadastrar (Pessoa model.nome model.idade) )



-- Lista pessoas ->
--     ( { model | listaPessoas = getPessoas pessoas }, Cmd.none )
-- VIEWS


view : Model -> Html Msg
view model =
    section [ class "hero is-primary is-fullheight is-bold" ]
        [ div [ class "hero-body", style "align-self" "center" ]
            [ div [ class "box" ]
                [ div [ class "field" ]
                    [ label [] [ text "Nome" ]
                    , div [ class "control" ]
                        [ input [ class "input is-rounded ", type_ "text", placeholder "Nome", Html.Attributes.value model.nome, onInput Nome ] []
                        ]
                    ]
                , div [ class "field" ]
                    [ label [] [ text "Idade" ]
                    , div [ class "control" ]
                        [ input [ class "input is-rounded ", type_ "text", placeholder "Idade", Html.Attributes.value (idadeIntToString model.idade), onInput Idade ] []
                        ]
                    ]
                , button [ class "button is-success is-rounded", Salvar model |> onClick ] [ text "Cadastrar" ]
                ]
            ]
        , div [ class "hero" ]
            [ div [ class "hero-body", style "align-self" "center" ]
                [ div [ class "box" ]
                    [ div [ class "field" ]
                        [ table [ class "table" ]
                            [ thead []
                                [ th [] [ text "Nome" ]
                                , th [] [ text "Idade" ]
                                ]
                            , tbody []
                                (List.map linhaPessoa model.listaPessoas)
                            ]
                        ]
                    ]
                ]
            ]
        ]


idadeIntToString : Int -> String
idadeIntToString idade =
    if idade == 0 then
        ""

    else
        String.fromInt idade


verificarStringInt : String -> Int
verificarStringInt string =
    if string == "" then
        0

    else
        string
            |> String.filter Char.isDigit
            |> String.toInt
            |> Maybe.withDefault 0


pessoaDecoder : Decoder Pessoa
pessoaDecoder =
    map2 Pessoa
        (field "nome" string)
        (field "idade" int)


getPessoas : String -> List Pessoa
getPessoas listaPessoas =
    case Json.Decode.decodeString (Json.Decode.list pessoaDecoder) listaPessoas of
        Ok pessoa ->
            pessoa

        Err _ ->
            []


linhaPessoa : Pessoa -> Html Msg
linhaPessoa pessoa =
    tr []
        [ td [] [ text pessoa.nome ]
        , td [] [ text (String.fromInt pessoa.idade) ]
        ]
