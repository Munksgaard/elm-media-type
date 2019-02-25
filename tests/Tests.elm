module Tests exposing (suite)

import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import MediaType
import Test exposing (..)


suite : Test
suite =
    describe "The MediaType module"
        [ describe "MediaType.fromString"
            [ test "application/javascript" <|
                \_ ->
                    MediaType.fromString "application/javascript"
                        |> Expect.equal
                            (Just
                                { parameters = Dict.empty
                                , registrationTree = Nothing
                                , subtype = "javascript"
                                , suffix = Nothing
                                , type_ = MediaType.Application
                                }
                            )
            , test "application/json" <|
                \_ ->
                    MediaType.fromString "application/json"
                        |> Expect.equal
                            (Just
                                { parameters = Dict.empty
                                , registrationTree = Nothing
                                , subtype = "json"
                                , suffix = Nothing
                                , type_ = MediaType.Application
                                }
                            )
            , test "application/x-www-form-urlencoded" <|
                \_ ->
                    MediaType.fromString "application/x-www-form-urlencoded"
                        |> Expect.equal
                            (Just
                                { parameters = Dict.empty
                                , registrationTree = Nothing
                                , subtype = "x-www-form-urlencoded"
                                , suffix = Nothing
                                , type_ = MediaType.Application
                                }
                            )
            , test "application/xml" <|
                \_ ->
                    MediaType.fromString "application/xml"
                        |> Expect.equal
                            (Just
                                { parameters = Dict.empty
                                , registrationTree = Nothing
                                , subtype = "xml"
                                , suffix = Nothing
                                , type_ = MediaType.Application
                                }
                            )
            , test "application/zip" <|
                \_ ->
                    MediaType.fromString "application/zip"
                        |> Expect.equal
                            (Just
                                { parameters = Dict.empty
                                , registrationTree = Nothing
                                , subtype = "zip"
                                , suffix = Nothing
                                , type_ = MediaType.Application
                                }
                            )
            , test "application/pdf" <|
                \_ ->
                    MediaType.fromString "application/pdf"
                        |> Expect.equal
                            (Just
                                { parameters = Dict.empty
                                , registrationTree = Nothing
                                , subtype = "pdf"
                                , suffix = Nothing
                                , type_ = MediaType.Application
                                }
                            )
            , test "application/sql" <|
                \_ ->
                    MediaType.fromString "application/sql"
                        |> Expect.equal
                            (Just
                                { parameters = Dict.empty
                                , registrationTree = Nothing
                                , subtype = "sql"
                                , suffix = Nothing
                                , type_ = MediaType.Application
                                }
                            )
            , test "application/graphql" <|
                \_ ->
                    MediaType.fromString "application/graphql"
                        |> Expect.equal
                            (Just
                                { parameters = Dict.empty
                                , registrationTree = Nothing
                                , subtype = "graphql"
                                , suffix = Nothing
                                , type_ = MediaType.Application
                                }
                            )
            , test "application/ld+json" <|
                \_ ->
                    MediaType.fromString "application/ld+json"
                        |> Expect.equal
                            (Just
                                { parameters = Dict.empty
                                , registrationTree = Nothing
                                , subtype = "ld"
                                , suffix = Just "json"
                                , type_ = MediaType.Application
                                }
                            )
            , test "application/msword" <|
                \_ ->
                    MediaType.fromString "application/msword"
                        |> Expect.equal
                            (Just
                                { parameters = Dict.empty
                                , registrationTree = Nothing
                                , subtype = "msword"
                                , suffix = Nothing
                                , type_ = MediaType.Application
                                }
                            )
            , test "application/vnd.openxmlformats-officedocument.wordprocessingml.document" <|
                \_ ->
                    MediaType.fromString "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
                        |> Expect.equal
                            (Just
                                { parameters = Dict.empty
                                , registrationTree = Just "vnd"
                                , subtype = "openxmlformats-officedocument.wordprocessingml.document"
                                , suffix = Nothing
                                , type_ = MediaType.Application
                                }
                            )
            , test "application/vnd.ms-excel" <|
                \_ ->
                    MediaType.fromString "application/vnd.ms-excel"
                        |> Expect.equal
                            (Just
                                { parameters = Dict.empty
                                , registrationTree = Just "vnd"
                                , subtype = "ms-excel"
                                , suffix = Nothing
                                , type_ = MediaType.Application
                                }
                            )
            , test "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" <|
                \_ ->
                    MediaType.fromString "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
                        |> Expect.equal
                            (Just
                                { parameters = Dict.empty
                                , registrationTree = Just "vnd"
                                , subtype = "openxmlformats-officedocument.spreadsheetml.sheet"
                                , suffix = Nothing
                                , type_ = MediaType.Application
                                }
                            )
            , test "application/vnd.ms-powerpoint" <|
                \_ ->
                    MediaType.fromString "application/vnd.ms-powerpoint"
                        |> Expect.equal
                            (Just
                                { parameters = Dict.empty
                                , registrationTree = Just "vnd"
                                , subtype = "ms-powerpoint"
                                , suffix = Nothing
                                , type_ = MediaType.Application
                                }
                            )
            , test "application/vnd.openxmlformats-officedocument.presentationml.presentation" <|
                \_ ->
                    MediaType.fromString "application/vnd.openxmlformats-officedocument.presentationml.presentation"
                        |> Expect.equal
                            (Just
                                { parameters = Dict.empty
                                , registrationTree = Just "vnd"
                                , subtype = "openxmlformats-officedocument.presentationml.presentation"
                                , suffix = Nothing
                                , type_ = MediaType.Application
                                }
                            )
            , test "application/vnd.oasis.opendocument.text" <|
                \_ ->
                    MediaType.fromString "application/vnd.oasis.opendocument.text"
                        |> Expect.equal
                            (Just
                                { parameters = Dict.empty
                                , registrationTree = Just "vnd"
                                , subtype = "oasis.opendocument.text"
                                , suffix = Nothing
                                , type_ = MediaType.Application
                                }
                            )
            , test "audio/mpeg" <|
                \_ ->
                    MediaType.fromString "audio/mpeg"
                        |> Expect.equal
                            (Just
                                { parameters = Dict.empty
                                , registrationTree = Nothing
                                , subtype = "mpeg"
                                , suffix = Nothing
                                , type_ = MediaType.Audio
                                }
                            )
            , test "audio/ogg" <|
                \_ ->
                    MediaType.fromString "audio/ogg"
                        |> Expect.equal
                            (Just
                                { parameters = Dict.empty
                                , registrationTree = Nothing
                                , subtype = "ogg"
                                , suffix = Nothing
                                , type_ = MediaType.Audio
                                }
                            )
            , test "multipart/form-data" <|
                \_ ->
                    MediaType.fromString "multipart/form-data"
                        |> Expect.equal
                            (Just
                                { parameters = Dict.empty
                                , registrationTree = Nothing
                                , subtype = "form-data"
                                , suffix = Nothing
                                , type_ = MediaType.Multipart
                                }
                            )
            , test "text/css" <|
                \_ ->
                    MediaType.fromString "text/css"
                        |> Expect.equal
                            (Just
                                { parameters = Dict.empty
                                , registrationTree = Nothing
                                , subtype = "css"
                                , suffix = Nothing
                                , type_ = MediaType.Text
                                }
                            )
            , test "text/html" <|
                \_ ->
                    MediaType.fromString "text/html"
                        |> Expect.equal
                            (Just
                                { parameters = Dict.empty
                                , registrationTree = Nothing
                                , subtype = "html"
                                , suffix = Nothing
                                , type_ = MediaType.Text
                                }
                            )
            , test "text/xml" <|
                \_ ->
                    MediaType.fromString "text/xml"
                        |> Expect.equal
                            (Just
                                { parameters = Dict.empty
                                , registrationTree = Nothing
                                , subtype = "xml"
                                , suffix = Nothing
                                , type_ = MediaType.Text
                                }
                            )
            , test "text/csv" <|
                \_ ->
                    MediaType.fromString "text/csv"
                        |> Expect.equal
                            (Just
                                { parameters = Dict.empty
                                , registrationTree = Nothing
                                , subtype = "csv"
                                , suffix = Nothing
                                , type_ = MediaType.Text
                                }
                            )
            , test "text/plain" <|
                \_ ->
                    MediaType.fromString "text/plain"
                        |> Expect.equal
                            (Just
                                { parameters = Dict.empty
                                , registrationTree = Nothing
                                , subtype = "plain"
                                , suffix = Nothing
                                , type_ = MediaType.Text
                                }
                            )
            , test "image/png" <|
                \_ ->
                    MediaType.fromString "image/png"
                        |> Expect.equal
                            (Just
                                { parameters = Dict.empty
                                , registrationTree = Nothing
                                , subtype = "png"
                                , suffix = Nothing
                                , type_ = MediaType.Image
                                }
                            )
            , test "image/jpeg" <|
                \_ ->
                    MediaType.fromString "image/jpeg"
                        |> Expect.equal
                            (Just
                                { parameters = Dict.empty
                                , registrationTree = Nothing
                                , subtype = "jpeg"
                                , suffix = Nothing
                                , type_ = MediaType.Image
                                }
                            )
            , test "image/gif" <|
                \_ ->
                    MediaType.fromString "image/gif"
                        |> Expect.equal
                            (Just
                                { parameters = Dict.empty
                                , registrationTree = Nothing
                                , subtype = "gif"
                                , suffix = Nothing
                                , type_ = MediaType.Image
                                }
                            )
            , test "text/html; charset=UTF-8" <|
                \_ ->
                    MediaType.fromString "text/html; charset=UTF-8"
                        |> Expect.equal
                            (Just
                                { parameters = Dict.fromList [ ( "charset", "UTF-8" ) ]
                                , registrationTree = Nothing
                                , subtype = "html"
                                , suffix = Nothing
                                , type_ = MediaType.Text
                                }
                            )
            , test "text/html+gzip;charset=UTF-8;foo=bar" <|
                \_ ->
                    MediaType.fromString "text/html+gzip;charset=UTF-8;foo=bar"
                        |> Expect.equal
                            (Just
                                { parameters =
                                    Dict.fromList
                                        [ ( "charset", "UTF-8" )
                                        , ( "foo", "bar" )
                                        ]
                                , registrationTree = Nothing
                                , subtype = "html"
                                , suffix = Just "gzip"
                                , type_ = MediaType.Text
                                }
                            )
            ]
        ]
