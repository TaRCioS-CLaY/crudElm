port module Main exposing (..)

import Browser exposing (..)
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (..)
import Json.Encode exposing (..)
import Task exposing (..)
import Time exposing (..)



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
    , nomeE : String
    , idadeE : Int
    , listaPessoas : Dict Int Pessoa
    , editandoPessoa : Int
    , apagandoPessoa : Int
    , horario : Posix
    , zonaHorario : Zone
    }


type alias Pessoa =
    { nome : String
    , idade : Int
    , creteadAt : Int
    }


init : String -> ( Model, Cmd Msg )
init pList =
    ( Model "" 0 "" 0 (Dict.fromList (getPessoas pList)) -1 0 (Time.millisToPosix 0) Time.utc, Task.perform Zona Time.here )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ pessoaApagada Apagar
        , Time.every 1000 Horario
        ]



-- Update


type Msg
    = Nome String
    | Idade String
    | NomeEditado String
    | IdadeEditada String
    | Salvar
    | Apagar Pessoa
    | ConfirmApagar Pessoa Int
    | Editar (Maybe Int) Pessoa
    | AceitaEdicao Int Pessoa
    | Cancelar
    | Horario Time.Posix
    | Zona Time.Zone



-- | PessoaApagadaJS Pessoa
-- | Lista String
-- port cadastrar : Pessoa -> Cmd msg
-- port editar : Pessoa -> Cmd msg


port apagar : Pessoa -> Cmd msg


port pessoaApagada : (Pessoa -> msg) -> Sub msg


port toJs : List Pessoa -> Cmd msg


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

        NomeEditado nome ->
            ( { model | nomeE = nome }, Cmd.none )

        IdadeEditada idade ->
            case String.toInt idade of
                Just idadeCerta ->
                    ( { model | idadeE = idadeCerta }, Cmd.none )

                Nothing ->
                    ( { model | idadeE = verificarStringInt idade }, Cmd.none )

        Salvar ->
            let
                pessoaNovaNaLista =
                    -- Pessoa model.nome model.idade (codigoPessoa model.listaPessoas) :: model.listaPessoas
                    Dict.insert (codigoPessoa model.listaPessoas) (Pessoa model.nome model.idade (Time.posixToMillis model.horario)) model.listaPessoas
            in
            ( { model | listaPessoas = pessoaNovaNaLista }, toJs (Dict.values pessoaNovaNaLista) )

        Apagar _ ->
            let
                pessoaExcluidaDaLista =
                    -- removePessoaLista pessoa.codigo model.listaPessoas
                    removePessoaLista model.apagandoPessoa model.listaPessoas
            in
            ( { model | listaPessoas = pessoaExcluidaDaLista }, toJs (Dict.values pessoaExcluidaDaLista) )

        ConfirmApagar pessoa codigo ->
            ( { model | apagandoPessoa = codigo }, apagar pessoa )

        Editar codigoParaEditar pessoa ->
            case codigoParaEditar of
                Just codigo ->
                    ( { model | editandoPessoa = codigo, nomeE = pessoa.nome, idadeE = pessoa.idade }, Cmd.none )

                Nothing ->
                    ( { model | editandoPessoa = -1 }, Cmd.none )

        AceitaEdicao codigoEdicao pessoa ->
            let
                pessoaEditadaNaLista =
                    Dict.update codigoEdicao (editaPessoa pessoa) model.listaPessoas

                -- editaPessoaLista codigoEdicao pessoa model.listaPessoas
            in
            ( { model | listaPessoas = pessoaEditadaNaLista, editandoPessoa = -1 }, toJs (Dict.values pessoaEditadaNaLista) )

        Cancelar ->
            ( { model | editandoPessoa = -1 }, Cmd.none )

        Horario hR ->
            ( { model | horario = hR }, Cmd.none )

        Zona z ->
            ( { model | zonaHorario = z }, Cmd.none )



-- PessoaApagadaJS pessoa ->
--     ( { model | listaPessoas = removePessoaLista pessoa.nome model.listaPessoas }, Cmd.none )
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
                , button [ class "button is-success is-rounded", Salvar |> onClick ] [ text "Cadastrar" ]
                ]
            ]
        , div [ class "hero" ]
            [ div [ class "hero-body", style "align-self" "center" ]
                [ div [ class "box" ]
                    [ div [ class "field" ]
                        [ input [ class "input is-rounded ", type_ "text", placeholder "Buscar" ] []
                        , table [ class "table" ]
                            [ thead []
                                [ th [] [ text "Nome" ]
                                , th [] [ text "Idade" ]
                                , th [] [ text "Data e Hora" ]
                                , th [] []
                                , th [] []
                                ]
                            , tbody [] <|
                                List.map
                                    (linhaPessoa model.editandoPessoa model)
                                    (Dict.toList
                                        model.listaPessoas
                                    )
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
    Json.Decode.map3 Pessoa
        (field "nome" Json.Decode.string)
        (field "idade" Json.Decode.int)
        (field "creteadAt" Json.Decode.int)


getPessoas : String -> List ( Int, Pessoa )
getPessoas listaPessoas =
    case Json.Decode.decodeString (Json.Decode.list pessoaDecoder) listaPessoas of
        Ok lista ->
            -- List.map (\p -> ( codigoPessoa (Dict.fromList (List.indexedMap Tuple.pair lista)), p )) lista
            lista
                |> List.indexedMap Tuple.pair

        Err _ ->
            []



-- linhaPessoa2 : Int -> Pessoa -> Html Msg
-- linhaPessoa2 editando pessoa =
--     tr []
--         [ td [] [ inputEdicaoNome pessoa editando ]
--         , td [ style "width" "6rem" ] [ inputEdicaoIdade pessoa editando ]
--         , td []
--             [ botaoEditarConfirmar pessoa editando
--             ]
--         , td []
--             [ botaoExcluirCancelar pessoa editando
--             ]
--         ]


linhaPessoa : Int -> Model -> ( Int, Pessoa ) -> Html Msg
linhaPessoa editando pModel ( codigo, pessoa ) =
    if editando == codigo then
        tr []
            [ td [] [ input [ class "input is-rounded is-small", type_ "text", placeholder "Nome", Html.Attributes.value pModel.nomeE, onInput NomeEditado ] [] ]
            , td [ style "width" "6rem" ] [ input [ class "input is-rounded is-small", style "width" "4rem", type_ "text", placeholder "Idade", Html.Attributes.value (String.fromInt pModel.idadeE), onInput IdadeEditada ] [] ]
            , td [] [ text (dataFormatada (Time.millisToPosix pessoa.creteadAt) pModel.zonaHorario ++ " ás " ++ horarioFormatado (Time.millisToPosix pessoa.creteadAt) pModel.zonaHorario) ]
            , td []
                [ button [ class "button is-small is-focused is-rounded is-info", AceitaEdicao codigo (Pessoa pModel.nomeE pModel.idadeE pessoa.creteadAt) |> onClick ]
                    [ text "Salvar" ]
                ]
            , td []
                [ button [ class "button  is-small is-focused is-rounded is-danger", Cancelar |> onClick ]
                    [ text "Cancelar" ]
                ]
            ]

    else
        tr []
            [ td [] [ text pessoa.nome ]
            , td [ style "width" "6rem" ] [ text (String.fromInt pessoa.idade) ]
            , td [] [ text (dataFormatada (Time.millisToPosix pessoa.creteadAt) pModel.zonaHorario ++ " ás " ++ horarioFormatado (Time.millisToPosix pessoa.creteadAt) pModel.zonaHorario) ]
            , td []
                [ button [ class "button is-small is-focused is-rounded is-info", Editar (Just codigo) (Pessoa pessoa.nome pessoa.idade 0) |> onClick ]
                    [ i [ class "fas fa-user-edit" ]
                        []
                    ]
                ]
            , td []
                [ button [ class "button  is-small is-focused is-rounded is-danger", ConfirmApagar pessoa codigo |> onClick ]
                    [ i [ class "fas fa-user-times" ]
                        []
                    ]
                ]
            ]


removePessoaLista : Int -> Dict Int Pessoa -> Dict Int Pessoa
removePessoaLista codigo lista =
    -- List.filter (\e -> e.codigo /= codigo) lista
    -- Dict.filter (\cod e -> cod /= codigo) lista
    Dict.remove codigo lista


editaPessoaLista : Int -> Pessoa -> Dict Int Pessoa -> Dict Int Pessoa
editaPessoaLista codigo edicaoPessoa lista =
    Dict.update codigo (\p -> p) lista


editaPessoa : Pessoa -> Maybe Pessoa -> Maybe Pessoa
editaPessoa pEditada pessoa =
    case pessoa of
        Just p ->
            Just { p | nome = pEditada.nome, idade = pEditada.idade }

        Nothing ->
            pessoa



-- Dict.map codigo (editaPessoa codigo edicaoPessoa) lista
-- editaPessoa : Int -> Pessoa -> Pessoa -> Pessoa
-- editaPessoa codigo pessoaAntiga pessoaEditada =
--     if pessoaAntiga.codigo == codigo then
--         { pessoaAntiga | nome = pessoaEditada.nome, idade = pessoaEditada.idade }
--     else
--         pessoaAntiga
-- inputEdicaoNome : Pessoa -> Int -> Html Msg
-- inputEdicaoNome pessoa codigo =
--     if codigo == pessoa.codigo then
--         input [ class "input is-rounded is-small", type_ "text", placeholder "Nome", Html.Attributes.value pessoa.nome ] []
--     else
--         text pessoa.nome
-- inputEdicaoIdade : Pessoa -> Int -> Html Msg
-- inputEdicaoIdade pessoa codigo =
--     if codigo == pessoa.codigo then
--         input [ class "input is-rounded is-small", style "width" "4rem", type_ "text", placeholder "Idade", Html.Attributes.value (String.fromInt pessoa.idade) ] []
--     else
--         text (String.fromInt pessoa.idade)
-- botaoEditarConfirmar : Pessoa -> Int -> Html Msg
-- botaoEditarConfirmar pessoa codigo =
--     if codigo == pessoa.codigo then
--         button [ class "button is-small is-focused is-rounded is-info" ]
--             [ text "Salvar" ]
--     else
--         button [ class "button is-small is-focused is-rounded is-info", Editar pessoa.codigo |> onClick ]
--             [ i [ class "fas fa-user-edit" ]
--                 []
--             ]
-- botaoExcluirCancelar : Pessoa -> Int -> Html Msg
-- botaoExcluirCancelar pessoa codigo =
--     if codigo == pessoa.codigo then
--         button [ class "button  is-small is-focused is-rounded is-danger", Apagar pessoa |> onClick ]
--             [ text "Cancelar" ]
--     else
--         button [ class "button  is-small is-focused is-rounded is-danger", ConfirmApagar pessoa |> onClick ]
--             [ i [ class "fas fa-user-times" ]
--                 []
--             ]


codigoPessoa : Dict Int Pessoa -> Int
codigoPessoa lista =
    if Dict.isEmpty lista then
        1

    else if Dict.size lista == 1 then
        2

    else
        Dict.keys lista
            |> List.sum



-- Funções Data e hora


dia : Posix -> Zone -> String
dia posix zona =
    String.fromInt (Time.toDay zona posix)


mes : Posix -> Zone -> String
mes posix zona =
    case Time.toMonth zona posix of
        Jan ->
            "01"

        Feb ->
            "02"

        Mar ->
            "03"

        Apr ->
            "04"

        May ->
            "05"

        Jun ->
            "06"

        Jul ->
            "07"

        Aug ->
            "08"

        Sep ->
            "09"

        Oct ->
            "10"

        Nov ->
            "11"

        Dec ->
            "12"


ano : Posix -> Zone -> String
ano posix zona =
    String.fromInt (Time.toYear zona posix)


hora : Posix -> Zone -> String
hora posix zona =
    String.fromInt (Time.toHour zona posix)


minuto : Posix -> Zone -> String
minuto posix zona =
    String.fromInt (Time.toMinute zona posix)


segundo : Posix -> Zone -> String
segundo posix zona =
    String.fromInt (Time.toSecond zona posix)


diaEHora : Posix -> Zone -> String
diaEHora posix zona =
    "Dia: " ++ String.fromInt (Time.toDay zona posix) ++ " Hora: " ++ String.fromInt (Time.toHour zona posix)


dataFormatada : Posix -> Zone -> String
dataFormatada posix zona =
    -- 21/04/2020 às 09:55
    dia posix zona ++ "/" ++ mes posix zona ++ "/" ++ ano posix zona


horarioFormatado : Posix -> Zone -> String
horarioFormatado posix zona =
    -- 21/04/2020 às 09:55
    hora posix zona ++ ":" ++ minuto posix zona ++ ":" ++ segundo posix zona



-- horarioAtual : Int
-- horarioAtual =
--     Time.posixToMillis (Task.perform Horario Time.now)
-- pegarHorario : Cmd Msg
-- pegarHorario =
--     Task.perform Horario Time.now
-- pegarZona : Zone
-- pegarZona =
--     Task.perform Horario Time.here
