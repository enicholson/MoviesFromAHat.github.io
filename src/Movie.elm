module Movie exposing (..)

import Html exposing (Html, div, img, text, a, button)
import Html.Attributes exposing (src, href, target, type_)
import Html.Events exposing (onClick)
import AppCss.Helpers exposing (class, classList)
import AppCss as Style
import Time.Date exposing (Date, day, month, year)
import Set exposing (Set)
import Genre exposing (Genre)


-- Types


type alias Movie =
    { title : String
    , url : String
    , img : String
    , year : Int
    , runtime : Int
    , genres : Set Genre
    , watched : WatchState
    }


type WatchState
    = Unwatched
    | Watched Date



-- Helpers


isWatched : Movie -> Bool
isWatched movie =
    case movie.watched of
        Unwatched ->
            False

        Watched _ ->
            True


watchDate : Movie -> Maybe Date
watchDate movie =
    case movie.watched of
        Unwatched ->
            Nothing

        Watched date ->
            Just date


matchGenres : Set Genre -> Movie -> Bool
matchGenres genres movie =
    case Set.size genres of
        0 ->
            True

        _ ->
            Set.size (Set.intersect genres movie.genres) > 0



-- Views


movieCard : (Maybe Movie -> msg) -> Set Genre -> Movie -> Html msg
movieCard focusMovie selectedGenres movie =
    let
        filtered =
            case Set.size selectedGenres of
                0 ->
                    False

                _ ->
                    Set.size (Set.intersect movie.genres selectedGenres) == 0
    in
        button
            [ classList
                [ ( Style.MovieCard, True )
                , ( Style.Filterable, True )
                , ( Style.Filtered, filtered )
                ]
            , Just movie |> focusMovie |> onClick
            , type_ "button"
            ]
            [ img
                [ class [ Style.Poster ]
                , src ("posters/" ++ movie.img)
                ]
                []
            , div
                [ class [ Style.Title ] ]
                [ text movie.title ]
            , notesView movie
            ]


notesView : Movie -> Html msg
notesView movie =
    case movie.watched of
        Unwatched ->
            div [ class [ Style.Notes ] ]
                [ text <|
                    (toString movie.year)
                        ++ ", "
                        ++ (toString movie.runtime)
                        ++ " min"
                ]

        Watched date ->
            div [ class [ Style.Notes ] ]
                [ text <|
                    (toString (month date))
                        ++ "."
                        ++ (toString (day date))
                        ++ "."
                        ++ (toString (year date))
                ]
