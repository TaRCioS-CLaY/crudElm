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
    , listaPessoas: List Pessoa
    }

type alias Pessoa =
    {nome : String
    , idade : Int
    }

init : String -> (Model, Cmd Msg)
init pList =
    (Model "" 0 (getPessoas pList) , Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ =
    pessoasCadastradas Lista

-- Update

type Msg
    = Nome String
    | Idade String
    | Salvar Model
    | Lista String


port cadastrar : Model -> Cmd msg

port pessoasCadastradas : (String -> msg) -> Sub msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Nome nome ->
            ({model | nome = nome} , Cmd.none)

        Idade idade ->
            case String.toInt idade of
                Just idadeCerta ->
                    ({model | idade = idadeCerta}, Cmd.none)
                Nothing  ->
                    ( {model | idade = verificarStringInt idade} , Cmd.none)

        Salvar pessoa ->
            (model, cadastrar pessoa)

        Lista pessoas ->
            ({model | listaPessoas = getPessoas pessoas} , Cmd.none)


-- VIEWS


view : Model -> Html Msg
view model =
    section [ class "hero is-primary is-fullheight is-bold" ] [
        div [ class "hero-body", style "align-self" "center"] [
            div [ class "box" ] [
                div [ class "field" ] [
                    label [] [text "Nome"]
                    , div [ class "control" ] [
                        input [ class "input is-rounded ", type_ "text", placeholder "Nome", Html.Attributes.value model.nome, onInput Nome] []
                        ]
                        ]
                        , div [ class "field" ] [
                            label [] [text "Idade"]
                            , div [ class "control" ] [
                                input [ class "input is-rounded ", type_ "text", placeholder "Idade", Html.Attributes.value (idadeIntToString model.idade), onInput Idade] []
                                ]
                                ]
                                , button [ class "button is-success is-rounded", Salvar {model | nome = model.nome, listaPessoas = model.listaPessoas, idade = model.idade} |> onClick] [text "Cadastrar"]
                                ]
                                , div [] []
                                , div [] [
                                    table [ class "table" ] [
                                        thead []
                                        [ th [][text "Nome"]
                                        , th [][text "Idade"]
                                        , tbody []
                                            (List.map linhaPessoa model.listaPessoas)
                                            ]
                                            ]
                                            ]
                                            ]
                                            ]


-- table []
--   [ tr []
--     [ th [] [ text "Título 1" ]
--     , th [] [ text "Título 2" ]
--     ]
--   , tr []
--     [ td [] [ text "Coluna 1"]
--     , td [] [ text "Coluna 2" ]
--     ]
--   ]



idadeIntToString: Int -> String
idadeIntToString idade =
    if idade == 0 then
        ""
    else
        String.fromInt idade

verificarStringInt: String -> Int
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

nomeDecoder : Decoder String
nomeDecoder =
    field "nome" string

idadeDecoder : Decoder Int
idadeDecoder =
    field "idade" int

getPessoas : String -> List Pessoa
getPessoas listaPessoas =
    case Json.Decode.decodeString pessoaDecoder listaPessoas of
        Ok pessoa ->
            [pessoa]
        Err _ ->
            []

-- linhasTabela: Model -> Html Msg
-- linhasTabela model =
--     List.map (linhaPessoa model) model.listaPessoas

linhaPessoa : Pessoa -> Html Msg
linhaPessoa pessoa =
    tr [] [
        td [] [text pessoa.nome]
        , td [] [text pessoa.nome]
        ]

