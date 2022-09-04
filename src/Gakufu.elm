module Gakufu005 exposing (main)

-- Achieved random list of music => Gakufu004.elm
-- next project is attach a button to show music sheet
-- Todo002.elm will work as reference
-- background color to each line

import Browser
import Html exposing (..)
import Html.Attributes as HA
import Html.Events as HE
import Random


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { jpgUrl : String
    , mp3Url : String
    , title : String
    , filename : String
    , dolist : Bool
    , list : List Musicdata
    }


type alias Musicdata =
    { jpgUrl : String
    , mp3Url : String
    , title : String
    , filename : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" "" "" "" True dict
    , Random.generate NewList (shuffle dict)
    )


type Msg
    = Submit
    | NewRandom Int
    | Shuffle
    | NewList (List Musicdata)
    | ShowMusic Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Submit ->
            ( model
            , Random.generate NewRandom (Random.int 1 (List.length dict))
            )

        NewRandom n ->
            case List.drop n dict of
                x :: _ ->
                    ( { model
                        | jpgUrl = x.jpgUrl
                        , mp3Url = x.mp3Url
                        , title = x.title
                        , filename = x.filename
                      }
                    , Cmd.none
                    )

                [] ->
                    ( model
                    , Cmd.none
                    )

        Shuffle ->
            ( model
            , Random.generate NewList
                (shuffle dict)
            )

        NewList newList ->
            ( { model | list = newList }
            , Cmd.none
            )

        ShowMusic index ->
            case List.drop index model.list of
                x :: _ ->
                    ( { model
                        | jpgUrl = x.jpgUrl
                        , mp3Url = x.mp3Url
                        , title = x.title
                        , filename = x.filename
                        , dolist = False
                      }
                    , Cmd.none
                    )

                [] ->
                    ( model
                    , Cmd.none
                    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


viewDetailedPdf : Model -> Html Msg
viewDetailedPdf model =
    div
        []
        [ img
            [ HA.src model.jpgUrl
            , HA.style "width" "100%"
            ]
            []
        ]


linecolor : Int -> String
linecolor index =
    if modBy 2 index == 0 then
        "pink"

    else
        "yellow"


view : Model -> Html Msg
view model =
    case model.dolist of
        True ->
            div
                [ HA.style "margin" "auto"
                , HA.style "width" "800px"
                ]
                (List.indexedMap
                    (\index music ->
                        p
                            [ HE.onClick (ShowMusic index)
                            , HA.style "background" (linecolor index)
                            ]
                            [ text music.title
                            , button
                                [ HA.style "float" "right"
                                ]
                                [ text "Show Music" ]
                            ]
                    )
                    model.list
                )

        False ->
            div []
                [ div
                    []
                    [ h1 []
                        [ audio
                            [ HA.src model.mp3Url
                            , HA.controls True
                            , HA.style "float" "right"
                            ]
                            []
                        ]
                    ]
                , div
                    [ HA.style "margin" "auto"
                    , HA.style "width" "90%"
                    ]
                    [ viewDetailedPdf model
                    ]
                ]


shuffle : List a -> Random.Generator (List a)
shuffle list =
    let
        randomNumbers : Random.Generator (List Int)
        randomNumbers =
            Random.list (List.length list) <| Random.int Random.minInt Random.maxInt

        zipWithList : List Int -> List ( a, Int )
        zipWithList intList =
            List.map2 Tuple.pair list intList

        listWithRandomNumbers : Random.Generator (List ( a, Int ))
        listWithRandomNumbers =
            Random.map zipWithList randomNumbers

        sortedGenerator : Random.Generator (List ( a, Int ))
        sortedGenerator =
            Random.map (List.sortBy Tuple.second) listWithRandomNumbers
    in
    Random.map (List.map Tuple.first) sortedGenerator


dict =
    [ { jpgUrl = "https://drive.google.com/uc?id=1dvVCvdFKKJlVmU3WqRgMkg0YsugL3qKR"
      , mp3Url = "https://drive.google.com/uc?id=1877DcOpz7rhRQ7XhpV7I34MQf018IDYt"
      , title = "水戸黄門(じんせいらくありゃくもあるさ)"
      , filename = "mitokomon.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1uCHqo0azmYAlA_uXPSbIDqONPcCCsktO"
      , mp3Url = "https://drive.google.com/uc?id=1uz6hK-iGgWwpW3o09bLO9ZRg8AQ3NVxC"
      , title = "スーダラ節(植木等、ちょいといっぱいのつもりでのんで)"
      , filename = "sudarabushi.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1Z2Qxq8UoHBDMv6wXuRmNMJWwryg2EN0z"
      , mp3Url = "https://drive.google.com/uc?id=1Mz6hlYoD-PUHQRuGVl1DDm3128sIIIKV"
      , title = "ママがサンタにキスをした(クリスマス。I Saw Mommy Kissing Santa Claus)"
      , filename = "mommykisssanta.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1qXQbr3mQNF3S-v4DhMvkiKLo5jKEnxFk"
      , mp3Url = "https://drive.google.com/uc?id=1hA4f6JyrXacIqCoOu98Ssb_5mLvyU6iT"
      , title = "ラスト・クリスマス(ワム！)"
      , filename = "lastchristmas.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1ldi4WgfnPKqNFh1ZnpOh4GLHoIKNCXed"
      , mp3Url = "https://drive.google.com/uc?id=1n6FeRm8jMkCnxQqx8sGnqW1LnZMy2ZB3"
      , title = "ジョニーが凱旋するとき(When Johnny Comes Marching Home)"
      , filename = "whenjohnny.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=13H_NvIKtfeFW_fRM21rU0LV5VS2K970w"
      , mp3Url = "https://drive.google.com/uc?id=1PEARIfK0ThavDeGT0KH7l0rbnKqPb4zN"
      , title = "ホワイト・クリスマス"
      , filename = "whitechristmas.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=10ZhxQ5_NiHQT9tAn7PHhqiNQ7AtzWxq8"
      , mp3Url = "https://drive.google.com/uc?id=17tul0wr1yBhuvCUdJjBuaLA1gpjMAJhP"
      , title = "おめでとうクリスマス(We Wish You a Merry Christmas)"
      , filename = "omedetouchristmas.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1ouKwNK7a8O1SbxiNA6754Mekw4wqbq9r"
      , mp3Url = "https://drive.google.com/uc?id=1kSlQD5D8cV1oXBpKtjMzmGzN2-hUuC_N"
      , title = "そりすべり(リロイ・アンダーソン。クリスマス)"
      , filename = "sorisuberi.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1w1LhB8aOftp1A3zOmlSQGjMye7GsowJI"
      , mp3Url = "https://drive.google.com/uc?id=1jvYKa16U5WI-Yo1sWxKkVgFXlxRWWupv"
      , title = "もろびとこぞりて(クリスマス)"
      , filename = "morobito.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1uhrOl8NYnOogep1FKU6BvOu67rrM_WC4"
      , mp3Url = "https://drive.google.com/uc?id=1KH0cab55b9HdFkbuz7XnLZvyhm6qgvKd"
      , title = "テネシーワルツ"
      , filename = "tennessee.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1cs8EtDp7rspgab-wc2SRypaDA_BtrLsW"
      , mp3Url = "https://drive.google.com/uc?id=1xU4mcYN6vjYW-IX5793FurYzR4DGrEpc"
      , title = "津軽海峡・冬景色(うえのはつのやこうれっしゃおりたときから)"
      , filename = "tsugaru.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1jpv7TUhae3NcmeOMebhfLYJu4bAEj6vG"
      , mp3Url = "https://drive.google.com/uc?id=1mEObQW8HP21XyA3CQrykrbPukem2f0TM"
      , title = "あら野のはてに(あらののはてにゆうひはおちて。クリスマス)"
      , filename = "aranonohateni.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1bRvWIAaz5jkCp2cF1dWILxHdF9roI3jq"
      , mp3Url = "https://drive.google.com/uc?id=1kDi3Uj0k1AsAIW404Z6vhktke11h_kIO"
      , title = "牧人ひつじを(まきびとひつじをまもれる。クリスマス)"
      , filename = "makibito.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1MHOF5ij89pecc636InrKXQyWd2qm8oyT"
      , mp3Url = "https://drive.google.com/uc?id=1XOLRMNqxCtGSYCMTCC_0T_3WfC60RIDv"
      , title = "ひいらぎかざろう(クリスマス)"
      , filename = "hiiragi.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1QDMCoj8CYbQ-rd-S2hPiafauRykr4w7R"
      , mp3Url = "https://drive.google.com/uc?id=1KR7Wd9MjJoUyvLvSsfvECZO2EpadaidE"
      , title = "涙そうそう(ふるいあるばむめくりありがとうってつぶやいた)"
      , filename = "nadasoso.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1cONlZ-VAnHRjaG-A6_nJviwRQt6q47Eb"
      , mp3Url = "https://drive.google.com/uc?id=1xz7jJpUCuhgpDcZmJ9jo1pl4vUF5O_Ie"
      , title = "アメイジング・グレイス"
      , filename = "amazinggrace.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=17y4yFh6UfyjTlQhjPH7fgdgWwj20oP1S"
      , mp3Url = "https://drive.google.com/uc?id=1jlDhEgmfEGUZSIA7StuHTz_esjN3Rb8e"
      , title = "夢はひそかに(ディズニー「シンデレラ」)"
      , filename = "dreamisawish.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=11nnSYaiShFBVFjnm-C-wXVuVr8zTVe7e"
      , mp3Url = "https://drive.google.com/uc?id=1XaG3H7bv9ftKFAzvnVvLsT2KWMqGLuP0"
      , title = "右から2番目の星(ディズニー「ピーター・パン」)"
      , filename = "migikara2banme.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1rHwKYFxld4fdO0tfBcfJRSjQgznWBxil"
      , mp3Url = "https://drive.google.com/uc?id=1lW7x2No_cMDxZgZz8eoh03SZ7NvBF2Au"
      , title = "花は咲く(まっしろなゆきみちにはるかぜかおる)"
      , filename = "hanawasaku.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1ysc2Bm8hSWUmqLmJRDn0mI8JRkSuZO7S"
      , mp3Url = "https://drive.google.com/uc?id=1VQG2ljNW7rGlG50O8nvoGvYweeIo5M5P"
      , title = "ライオンは寝ている(トークンズ)"
      , filename = "lionsleeps.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1YW3ASSfbO8RFLdkGWwF83mC2RJHQlKTm"
      , mp3Url = "https://drive.google.com/uc?id=17h58N9AFOuyCC6Mns-sVjwPoIMBqV5ag"
      , title = "ラ・ラ・ルー(ディズニー「わんわん物語」)"
      , filename = "lalalu.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1VxPJtDXF08Q-QCnRz73_jqQ9BDNeivRL"
      , mp3Url = "https://drive.google.com/uc?id=1ygAkAE49i1yYSlRqiqiB4In_b-xT5POo"
      , title = "ドラゴンクエスト序曲"
      , filename = "dragonquest.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1ZLNMXjLhbtx1E6G2yen5I0O7xFDpAMWO"
      , mp3Url = "https://drive.google.com/uc?id=1cgdOkILV2stvb2TqTau0Z0-OOCdKyX_M"
      , title = "春の唄(らららあかいはなたば)"
      , filename = "harunouta.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1qxvdZrrnWJBsuIJcH6XSZrXtvMyI6z8y"
      , mp3Url = "https://drive.google.com/uc?id=1v-hAoSoiuNoZNaFmJLCqJ67IcoOsBqdd"
      , title = "シンコペーテッド・クロック(ルロイ・アンダーソン)"
      , filename = "syncopatedclock.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1qLVBGoVXIivoP98uAvn3yxF1tK6hoF56"
      , mp3Url = "https://drive.google.com/uc?id=11TFnETlYYEEhpcf_X7ClfuRW3xnDQ7w0"
      , title = "オネスティ(ビリー・ジョエル)"
      , filename = "honesty.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=15ETizs6k2i0-DOzVrGVL7stHTNZKNrWV"
      , mp3Url = "https://drive.google.com/uc?id=1DCKvW0_W3slyIBUyYw1Y902gLCAD-nAZ"
      , title = "ビッグ・ベンの鐘(ウェストミンスター宮殿の時計)"
      , filename = "bigben.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1lNT8N9TuQEgJH840sp4j19LbILyv-D2C"
      , mp3Url = "https://drive.google.com/uc?id=1BhHv9LrNPP72BjieWt6VHUSji9vkWGTG"
      , title = "恋は水色(ポール・モーリア)"
      , filename = "lamourestbleu.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1-wOeFLfqnVM96CJ5nz6Bav4mC00QfB0K"
      , mp3Url = "https://drive.google.com/uc?id=1vyTgp3Pe3vJL7f2NeY77_uNmcXG0TCQ9"
      , title = "ポリリズム(Perfume とてもだいじなきみのおもいは)"
      , filename = "polyrhythm.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1Q-qvIUB4vJhojte5Gl4TcebEfnP2XoXs"
      , mp3Url = "https://drive.google.com/uc?id=1ULAQpP62VcW3oGR4LcZzgS-7-yutIsVk"
      , title = "春よ、来い(松任谷由美。あわきひたりたつにわかあめ)"
      , filename = "haruyokoimatsutoya.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=167ShNKUA5n2FmUB5k5IhMu02XRZvmuJY"
      , mp3Url = "https://drive.google.com/uc?id=1_XRfq3pQyshxhjGk4jnxGEGp3XNzj-tz"
      , title = "浪花節だよ人生は(のめといわれてすなおにのんだ)"
      , filename = "naniwabushi.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1LMibWxQMAnQQzel6SMeQJzNMeaCab2Fs"
      , mp3Url = "https://drive.google.com/uc?id=1xvUEHUkyZ566gXRaSgNhxQ8QeT-nmWe1"
      , title = "マルエツの歌(どくたーげんきどくたーげんき)"
      , filename = "maruetsu.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1SOgvT204wgKf8ZOm0L1sVl85sjI8druw"
      , mp3Url = "https://drive.google.com/uc?id=13EUkyyxzgmZvdSIkxhhG-kce5rsL0Gh-"
      , title = "ローソンストア100(ひゃくひゃくひゃくえん)"
      , filename = "lawsonhyaku.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1J24Drfi9KqQ8r52P_qnXdfDHdufXcS1F"
      , mp3Url = "https://drive.google.com/uc?id=1s4yIJlA9llHSjFhEWB4-0zNDO0VguDHt"
      , title = "ケーズデンキの歌(ゆめゆめはっぴーいつでもやすい)"
      , filename = "ksdenki.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1n4j-GgKcCdbJh6N65PEA_objdUznXmVo"
      , mp3Url = "https://drive.google.com/uc?id=10kEgfV2KGjM9oWWaB10wnz1W8asNyPuP"
      , title = "セサミストリートのテーマ(さーにーでい)"
      , filename = "sesamistreet.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=14vFMATcmJ9XV6h_TAEPmVcvdcNfsIWO8"
      , mp3Url = "https://drive.google.com/uc?id=1IZYVIRAMdYTxTl4buyZpEgU8MlBohZz8"
      , title = "ハート・アンド・ソウル(Heart and Soul)"
      , filename = "heartandsoul.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=14rSEr5sGqWguJVfCjOqUN3niHthbww0s"
      , mp3Url = "https://drive.google.com/uc?id=187rB_4IbKxlySnLWvOLKotNwPCLlfc_t"
      , title = "東京節(パイノパイノパイ)"
      , filename = "tokyobushi.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1Q-upRUVCBtjYMzn_Cv19aya7U7-QgbLi"
      , mp3Url = "https://drive.google.com/uc?id=14sJKZB5sU_97W98x9DQr4IaeE2iNICQ8"
      , title = "行商人(コロブチカ、korobeiniki, korobushka)"
      , filename = "korobeiniki.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1a1mn2DXPQaJxrBudJhZw7iWVqU_AgOaE"
      , mp3Url = "https://drive.google.com/uc?id=1fkBoUIZ_yISrn-4qcnICromLiYbqTG44"
      , title = "蒲田行進曲(にじのみやこひかりのみなときねまのてんち)"
      , filename = "kamata.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1_xVaabYUbL7M1VNVguYoAYLo2DLf5sZS"
      , mp3Url = "https://drive.google.com/uc?id=1jzDlfFfDi3or0B5MR-oonA2okFHFkMjd"
      , title = "カリンカ"
      , filename = "kalinka.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1A0CJZcuio21j0_9fUL_04DTT0g9vDkl6"
      , mp3Url = "https://drive.google.com/uc?id=1c-OhnN2zG00F2MNhIieF5ULWUc4oHyLY"
      , title = "君は我が心の中に(Du, Du Liegst Mir Im Herzen)"
      , filename = "duliegstmir.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1XcrL82FKCba7bM487s7PNMmsNUI4REFE"
      , mp3Url = "https://drive.google.com/uc?id=1RQlxrWBJVr70okBGvml5Z6nkb1bLBGeF"
      , title = "冬のソナタ(最初から今まで )"
      , filename = "fuyunosonata.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1jE0-N9KOGFq8Wok-CzplCIyvhI_jXvfA"
      , mp3Url = "https://drive.google.com/uc?id=1e5SNtOTP_PV5xyPiU0zJyxSUJTqKm3Y0"
      , title = "いつも何度でも(千と千尋の神隠し。よんでいるどこかむねのおくで)"
      , filename = "itsumonando.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1Z2kMe0ghb6jC5Uxuv65KCq_ygY3LttPG"
      , mp3Url = "https://drive.google.com/uc?id=1UBsVhZHAFJZp0TXFAFin46CKQ8hrgL8Q"
      , title = "主よ人の望みの喜びよ(J.S.バッハ)"
      , filename = "shuyohitononozomino.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1jm6PhloLJV6GrcLYsZ6CsvyzHz2hBQbj"
      , mp3Url = "https://drive.google.com/uc?id=1kkHN0Mira_KfuGNOR-0dNJNuAAdyQVyT"
      , title = "ありのままで(アナと雪の女王イントロ。let It Go)"
      , filename = "letitgointro.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1zYnzkMg3wuTLmr7LjskscLGsO1EQou3l"
      , mp3Url = "https://drive.google.com/uc?id=1Zj84PnbjxCJ6stycApbUUpRVexGJKl_U"
      , title = "ます(シューベルト)"
      , filename = "masu.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1ib49mMTNbJqrPOStCxNWqwaIFT758XTb"
      , mp3Url = "https://drive.google.com/uc?id=1w7yAe4dKezP1tZn7GssP-YcaO1MN6-Vh"
      , title = "華麗なる大円舞曲(ショパン)"
      , filename = "kareinaru.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1zgnlJbF9fyF6uXkx_jEG0PX3d2DhM2bV"
      , mp3Url = "https://drive.google.com/uc?id=111giGLdFiDE8pvVnfwP31gq04M5vPXH7"
      , title = "天国と地獄(オッフェンバック)"
      , filename = "tengokujigoku.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=17x2M9193-I44D2yJrZ6PZkMpSyQLOJOj"
      , mp3Url = "https://drive.google.com/uc?id=17pYZnZMuBo4CCh_gCeWdCBmyZ2z0U86h"
      , title = "クシコス・ポスト(ネッケ)"
      , filename = "csikospost.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1i5vt1PIXpPTxyurhwlr-3KZGRzTmYH74"
      , mp3Url = "https://drive.google.com/uc?id=16OlB-FsTnY-8R1mE5SX_j6-KxzRdFglw"
      , title = "恋とはどんなものかしら(モーツアルト。フィガロの結婚より)"
      , filename = "koitowadonna.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1S4i8guOwAti_55LgtJ09StN8uT15G6DQ"
      , mp3Url = "https://drive.google.com/uc?id=1eH-C9NqTNGKY9coURlKNMJdCMUP9U7H1"
      , title = "ジムノペディ1番(サティ)"
      , filename = "gymnopedies.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1UuOwx135urMNvk5obIJ4X_NI19VeJzee"
      , mp3Url = "https://drive.google.com/uc?id=1MxPKtxYwK8K5XnbGDi2Hq03smCrTrPvo"
      , title = "亜麻色の髪の乙女(ドビュッシー)"
      , filename = "amairodebussy.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1tcxz356pIlhJGuxq8GYv8Sx8ESmNLCt_"
      , mp3Url = "https://drive.google.com/uc?id=11cXjM6Vhh89c0G4AZFR1R940gb7cM8js"
      , title = "美しき青きドナウ(ヨハン・シュトラウス2世)"
      , filename = "donau.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=16az8ORiv3_Gkn1i27gWGGiWcAmA8xfqs"
      , mp3Url = "https://drive.google.com/uc?id=1BbI5uYSih3ZN9-1_9pzkzc9QF6eofXeQ"
      , title = "ジュ・トゥ・ヴ(エリック・サティ)"
      , filename = "jeteveux.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1VIRoFEuz4isusxWFeA7VzdVwNFbs25sV"
      , mp3Url = "https://drive.google.com/uc?id=1oFARTSCKkwL-DXduZ6C25IgH5bunpmeR"
      , title = "モーツァルトの子守歌"
      , filename = "mozartkomori.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1KabE7PO1-SUgXNgCP8GOL2xPaDXC8kPh"
      , mp3Url = "https://drive.google.com/uc?id=1xoFNN2tJ1cd-OXrbqIoWJuT2CnJdT9eP"
      , title = "ブラームスの子守歌"
      , filename = "brahmskomori.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1VVdq0KrKjYxbkgycYhm02cKjism8HHIh"
      , mp3Url = "https://drive.google.com/uc?id=1mZXkhY9tGiwf9POwdjVvSmZqJqzJqFty"
      , title = "運命(ベートーベン交響曲5番)"
      , filename = "unmei.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1CDaVU0XjiFnDFhriij0eQAEQdU_KPiFP"
      , mp3Url = "https://drive.google.com/uc?id=1ZR0JA3gj7ATEAAQiomElGSwL-Z0rFdZo"
      , title = "ハバネラ(ビゼー。カルメンより)"
      , filename = "habanera.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1_Ac3Rk8CJH5zUoM2Zx3nXddoYOQ8PRAX"
      , mp3Url = "https://drive.google.com/uc?id=1Ty10QEffZ-hxMAbKf8q51qm_0k6SQ9G3"
      , title = "新世界(ドヴォルザーク)"
      , filename = "shinsekaip.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=11fH3Ekzgv7vCcrROlRlqa5KDXoc6ZxG1"
      , mp3Url = "https://drive.google.com/uc?id=1G4EP0M7qIpGXraaDIrzOW6YLcfU7c-KA"
      , title = "ヴィヴァルディ四季より春"
      , filename = "vivaldishiki.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1Z-XRzaWNiZlAYFfAqgdTM-OoDPWBqyWi"
      , mp3Url = "https://drive.google.com/uc?id=1EUWUhemlvRwYKC1cHi0XCaZPpg7-l7AL"
      , title = "威風堂々(エルガー)"
      , filename = "ifudodo.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1A7n3h7KqnGfL_7R2qsXFTFkgEicu46AF"
      , mp3Url = "https://drive.google.com/uc?id=1eIGiSxWo9kLbSiIr1Aj8myvpn7Hj7S2S"
      , title = "春の歌(メンデルスゾーン)"
      , filename = "mendelsharunouta.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1R-_oBgN-uo9Umt58dj_bDt98TJZuIi4I"
      , mp3Url = "https://drive.google.com/uc?id=1HkbtqUxqnNhFHY7rCznAyJHiC2jxvfjp"
      , title = "乾杯の歌(ヴェルディ)"
      , filename = "kanpai.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1u2eLbZdoFMFo6OT9R9k_sHXq5OoLXJTc"
      , mp3Url = "https://drive.google.com/uc?id=1qwtV9HR-AsPfNeM-jLgUs-1_rmGjtxlr"
      , title = "ラデツキー行進曲(ヨハン・シュトラウス1世)"
      , filename = "radetzky.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1tMfPasJu9scYLlyg7QE0wajJESRX0bXv"
      , mp3Url = "https://drive.google.com/uc?id=1GpNrRtmi-y9lzmGYTl36PkP2Oy-_1nXT"
      , title = "ずいずいずっころばし"
      , filename = "zuizui.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1DW8gcnWRffG6ZE2adEEq7wF0Q-8INVWv"
      , mp3Url = "https://drive.google.com/uc?id=1BHOn481oLb5q-dC8iYRnqDr4gE-fhFJ1"
      , title = "燃えろよ燃えろよ"
      , filename = "moeroyo.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1lbz-a988jDc9GXe0SVgdOJnT0D4T-its"
      , mp3Url = "https://drive.google.com/uc?id=1GHAmUg3ZsKvC8O5mAip9wqeZAB9-d3G9"
      , title = "マイボニー(My Bonnie Lies Over the Ocean)"
      , filename = "mybonnie.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1v3SCofTxg2gYNP13iIp9TkcHAemOGBX-"
      , mp3Url = "https://drive.google.com/uc?id=1s5yw81tNyUCM_Qz3IP_gisKD9r8AkSTT"
      , title = "茶色の小瓶"
      , filename = "chairo.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1y0CN0x6t0uBKWbR5Yp18jGixdpRXoNRE"
      , mp3Url = "https://drive.google.com/uc?id=1Z0Chk-3kM4GclVhMjTPhpo7JhruPFwr8"
      , title = "権兵衛さんの赤ちゃん(ごんべえさんのあかちゃんが)"
      , filename = "gonbe.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1-n4psZwGJAq-Sxx58WwoRISq3quJALOL"
      , mp3Url = "https://drive.google.com/uc?id=1v01I0ZC09yK7SrBto1wJnab-ipDmUUmb"
      , title = "山の音楽家(わたしゃおんがくかやまのこりす)"
      , filename = "yamanoongakuka.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1CId-_uO92l_R1ciCHKHOlALk4Q4Zojk0"
      , mp3Url = "https://drive.google.com/uc?id=10S5zMSbW1bxHbcQYn0QR66D77-ytaR7c"
      , title = "木星(ホルスト「惑星」よりジュピター)"
      , filename = "mokusei.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=16AsfJ_6Y2w6fpo969gSa9Rfd_tLYO-XK"
      , mp3Url = "https://drive.google.com/uc?id=1USdA-PXDzPd-Gnbw0nkZigIk8ixt415d"
      , title = "アイネ・クライネ・ナハト・ムジーク(モーツァルト)"
      , filename = "eineclinenacht.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1uCDdmklyq3uDkqsu3xWM2WUxOfoUs90s"
      , mp3Url = "https://drive.google.com/uc?id=1TaQVLcgr3iquaoLhmaiI1E1TmaddFS1L"
      , title = "雨だれの前奏曲(ショパン)"
      , filename = "amadare.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1JqZYLTvd1346IF69lemTFe1I6HCtHhcg"
      , mp3Url = "https://drive.google.com/uc?id=1NWEYaFtt2Imu-JxfwxwtO4bVamtJLJOc"
      , title = "愛の喜び(マルティーニ)"
      , filename = "ainoyorokobi.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1mH5GBbbKVGVN0hLOLzg-94ultqiN8a50"
      , mp3Url = "https://drive.google.com/uc?id=1qGfklxN1OMgfl6Oe2hOv9guJtv9gzpVa"
      , title = "江戸の子守唄(ねんねんころりよおころりよ)"
      , filename = "edokomori.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1UKe-y0iaTrz68KKbPA-xFXxqgqpmbWRB"
      , mp3Url = "https://drive.google.com/uc?id=1t2rvtaHQjn6v9RhOcFINbMAa2Jsr--31"
      , title = "あんたがたどこさ(ひごさひごどこさくまもとさ)"
      , filename = "antagata.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=12Dnp3TcPlVFK-tIK_t6xeAEclhlO9tmy"
      , mp3Url = "https://drive.google.com/uc?id=1shHTsftfJiv-HEmA0ebWQ-z2KWdL69SK"
      , title = "森のくまさん(あるひもりのなかくまさんにであった)"
      , filename = "morinokuma.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1-7sowp8_dvpF0fJ50zyEw7qsnDjPeJGB"
      , mp3Url = "https://drive.google.com/uc?id=1ZjV1gdDA2MCxg13_oiBeZP9Aqfwr111l"
      , title = "ブラームスのワルツ(円舞曲)"
      , filename = "waltzbrahms.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1noFwx_rdZJO9iJZRAg610GqjsGUZMYBh"
      , mp3Url = "https://drive.google.com/uc?id=1T3NVl2cE1-2wIr4RcZ0wzmexnsnW-4ny"
      , title = "雪(ゆきやこんこあられやこんこ)"
      , filename = "yukiyakonko_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=18DshRABxNP6LHf3qcpLL2c4oMX6XZRaW"
      , mp3Url = "https://drive.google.com/uc?id=15Dskw1ZfGt9luey8zVQzLfGm4vFY_06o"
      , title = "北国の春(しらかばあおぞらみなみかぜ)"
      , filename = "kitaguninoharu_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1wuYPF84MpyFmL5Mrd5DMbjPu8k6qma4s"
      , mp3Url = "https://drive.google.com/uc?id=1842fGkGa84TPaTIv0wY3BJ-B_g0bW725"
      , title = "静かな湖畔(しずかなこはんのもりのかげから)"
      , filename = "shizukanakohan_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1qpcJisVQ030gQ29Oh0kozDhj78P7ZYm4"
      , mp3Url = "https://drive.google.com/uc?id=1-n_O43SDQVsCL1TfIrMnKBB0t9-Ap8B8"
      , title = "女のみち(わたしがささげたそのひとに)"
      , filename = "onnanomichi_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1qBn8rz0k4PPESJ5YceKEEmdP3fH2D0xc"
      , mp3Url = "https://drive.google.com/uc?id=1S2cSyO2u870rKDCauhm677VTpMBaRhuo"
      , title = "おもちゃのチャチャチャ"
      , filename = "omochanochachacha_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1Wg6NzmAt0c82rN57WBJWm5FEbqAGlBNe"
      , mp3Url = "https://drive.google.com/uc?id=1xZMtjeOdzSiKGeRf6R06fZMlLO4mKfF_"
      , title = "ペールギュントより朝(グリーグ)"
      , filename = "peergyntasagrieg_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=153Xoh2BXwupoyUgCOOfQhVRD0tRYUWfm"
      , mp3Url = "https://drive.google.com/uc?id=1ak_0vasbM215p6mcbbcvN2Sfl-hxt1UP"
      , title = "ホルン協奏曲第1番(モーツァルト)"
      , filename = "hornmozart_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1ACyCgD0JIbIvdSDXfV20qkIrNie9Ti--"
      , mp3Url = "https://drive.google.com/uc?id=1m3y7JvvBylzVWxDcdPMHW4uLmi-RkCOU"
      , title = "池の雨(ヤマハ音楽教室幼児科メロディー暗唱曲)"
      , filename = "ikenoame_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=10jPVYnapneZaz0Z3DhAAtLwM_Y2JuShL"
      , mp3Url = "https://drive.google.com/uc?id=1BnK43j7Izorjld6wfR_KjOKUxx2GS0I_"
      , title = "ベートーベンのトルコ行進曲"
      , filename = "turkbeethoven_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1gmwB1G5HWfg7DgqDNz2UA_0Zi7GM_rgo"
      , mp3Url = "https://drive.google.com/uc?id=1s4xdrVVv9DIDyrts8qY85efbWuq0Yu12"
      , title = "聖者が街にやってくる(聖者の行進)"
      , filename = "seija_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1dUxbsTNsJfDTOaiLtU45-s-2vYCc0owr"
      , mp3Url = "https://drive.google.com/uc?id=1qYT5CvoOyHHBWXxABjOk3rQVPK1H9ud-"
      , title = "こぎつね(こぎつねこんこんやまのなか)"
      , filename = "kogitsune_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1J4V9tUKuVL6Q3y_a7hHL0CXldoyrv5q3"
      , mp3Url = "https://drive.google.com/uc?id=1MYR7yWYunHmSkH6m7W46XnWnVjgRMUPd"
      , title = "ロンドン橋(ろんどんばしおちた)"
      , filename = "londonbashi_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1f0NYHiLK-w6KXo0safnfO4CTP2CQbh6M"
      , mp3Url = "https://drive.google.com/uc?id=115WGuy--x3DYjkU3P0QSWzBQQHRFkq6F"
      , title = "メリーさんの羊(めりーさんのひつじ)"
      , filename = "marysanno_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1hCzT-6C1ZCWi7Spiu_ea0Pxm1cYZ0mvc"
      , mp3Url = "https://drive.google.com/uc?id=1KOVLjXQVFIb2N451H6OZwlai5Ij6BUNt"
      , title = "アブラハムの子(あぶらはむにはしちにんのこ)"
      , filename = "abrahamunoko_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1gI55GATU1X6LYliP8GrYzHbSdeNqXHpE"
      , mp3Url = "https://drive.google.com/uc?id=18sn4HRldIKMaAq0EPeLCJ4gsmZlMHcVU"
      , title = "茶摘(ちゃつみ。なつもちかづくはちじゅうはちや)"
      , filename = "chatsumi_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1RtMAoXrtO4K09fjrzaKucixS-KzNVZ_k"
      , mp3Url = "https://drive.google.com/uc?id=17lUw9X8oWnMtrOV9iP_NcFOCjT2JEpzL"
      , title = "大きな古時計(おおきなのっぽのふるどけい)"
      , filename = "okinafurudokei_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1giwkfoVHW0oIwz2aK-hFHZu02_7adB8S"
      , mp3Url = "https://drive.google.com/uc?id=1VQY3djnR06RpibM4R_3tZzX5LfmRvtO8"
      , title = "たき火(かきねのかきねのまがりかど)"
      , filename = "takibi_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1HOf_4CuzXjOuZIo5kI8tcwm0Ge7tGKo6"
      , mp3Url = "https://drive.google.com/uc?id=1j7cLWnJ0BJQupkOXJhOtHGstO_ELiPOL"
      , title = "かたつむり(でんでんむしむし)"
      , filename = "katatsumuri_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1aWOgRQhdWADXpPLH9PBvcWrrwebNgkFh"
      , mp3Url = "https://drive.google.com/uc?id=1cm0S5tGC-yUK7sYuFP_knB9lAXB8IWfh"
      , title = "ほたるこい"
      , filename = "hotarukoi_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1qTEnCelZoycCd69i4OlKD_fZdXlVtBGP"
      , mp3Url = "https://drive.google.com/uc?id=19OjobdnGuYn96yuNmvcOlvqVEofBrW3n"
      , title = "かごめかごめ(かごのなかのとりは)"
      , filename = "kagome_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1flKwKu9EzJs2AxP2Uw_zqvJ3AtaRtBnf"
      , mp3Url = "https://drive.google.com/uc?id=1e4LEm4DXUaqEyMCqLdHcgXKrE6M7vZyS"
      , title = "かえるの合唱(かえるのうたがきこえてくるよ)"
      , filename = "kaerunogassho_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1bWr6oICBWPLPzW2dUFYWUGW5mC29Si24"
      , mp3Url = "https://drive.google.com/uc?id=1dOb0Zd-bV1yfwKkVvANDZmy-wDGW1M1A"
      , title = "ゆかいな牧場(いちろうさんのまきばでいーあいいーあいおー)"
      , filename = "yukainamakiba_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1V9xOfDwD6yGGGcAUWSb_udnlDiuGgvkC"
      , mp3Url = "https://drive.google.com/uc?id=1u5VNeym1x_1TSo8LRKhcN5F4aUX4NMJ0"
      , title = "ぶんぶんぶん(ぶんぶんぶんはちがとぶ)"
      , filename = "bunbunbun_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1Ez4BeywMtfrBTlAR6GtfHY7FClCaijj0"
      , mp3Url = "https://drive.google.com/uc?id=1Cd05l-iihpVT_a5zeXFNtggvTa7UZVyd"
      , title = "大きな栗の木の下で(おおきなくりのきのしたで)"
      , filename = "okinakuri_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1KzatRA02xsN9LsrBbPFSPm7Kg9b4S7kt"
      , mp3Url = "https://drive.google.com/uc?id=1n4vPS1PVcR09hIeEnngs4YzLw6yMxfEb"
      , title = "とんぼのめがね"
      , filename = "tonbono_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1Qe4tiRgEtZv-XVrUf9C9PuucAEM4MsuJ"
      , mp3Url = "https://drive.google.com/uc?id=186fahMimqSAbgc-24_R_ogjsrerNVfXw"
      , title = "お正月(もういくつねるとおしょうがつ)"
      , filename = "oshogatsu_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1dtrqgTM1a_FuGcRS7pCQ45m6tpK2XG4j"
      , mp3Url = "https://drive.google.com/uc?id=1C-FH8z0sjNk34NK_CwG5YmtJ50ngkAfg"
      , title = "凱旋行進曲(ヴェルディ。アイーダ)"
      , filename = "gaisen_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1jsm43y2et47tEXW5XiVEgqwRynF3tmMq"
      , mp3Url = "https://drive.google.com/uc?id=1wfPPWpGdQwjB7QcWXIm1qZwt6-YN7Lno"
      , title = "Ob-La-Di, Ob-La-Da (ビートルズ)"
      , filename = "obladi_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1gi9N06Fn5X3JoKpTKPv8FGq33BqgtrfA"
      , mp3Url = "https://drive.google.com/uc?id=1iVF-9H9cuc9hkrT3y3JA6gRgTGYGROsJ"
      , title = "Carry That Weight (ビートルズ)"
      , filename = "carrythatweight_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1YkNtnNZ8D2XUmc_pEVNT0-2wAbSjB45W"
      , mp3Url = "https://drive.google.com/uc?id=1dD-LpMMBPgnXhcrL7RzrUiv02ihFV_6t"
      , title = "Across the Universe (ビートルズ)"
      , filename = "acrossuniverse_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1mP_OM3oWM1SpzrH8J9N00ZiHXZwKFHhD"
      , mp3Url = "https://drive.google.com/uc?id=1guwbOZo8bSfw66PyXC2t4OdPViKtadRN"
      , title = "有楽町で逢いましょう(あなたをまてばあめがふる)"
      , filename = "yurakucho_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1w758ucrQ8iOGBNe58Su4I-thIUkbk5lr"
      , mp3Url = "https://drive.google.com/uc?id=1wQvReBXq6S9yNk0qGrTqSjsERIFraU1T"
      , title = "秋桜(うすべにのこすもすがあきのひの)"
      , filename = "cosmos_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1dKzyqi2R-BRWpB1urm1yYtRjy6KQrqia"
      , mp3Url = "https://drive.google.com/uc?id=1W3Uj4G1xupngrcJjI773Nu7wefJXAl7U"
      , title = "だんご３兄弟(くしにささってだんごだんご)"
      , filename = "dango3kyodai_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1gBrFQXn7493yX1wU6KUF1KabtxsRKLby"
      , mp3Url = "https://drive.google.com/uc?id=1jOdhUTybguaIK2O-25NrD7bxqkClKIII"
      , title = "チム・チム・チェリー(ちむちむにーちむちむにー)"
      , filename = "chimchimcheree_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1JYXpUnLFgTyht62TFMCq-JePJJEPSWCS"
      , mp3Url = "https://drive.google.com/uc?id=1XpVxzqAUwjglsqW_3s-_b1gn4LL_pPss"
      , title = "夜霧よ今夜もありがとう(しのびあうこいをつつむよぎりよ)"
      , filename = "yogiriyo_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1dknMDQb0ovZlIKfOZvO-VGQhMTdLj7qG"
      , mp3Url = "https://drive.google.com/uc?id=1GcyV-v2HA1z9UZoWV0dE9EHiadxIVk6r"
      , title = "星の流れに(ほしのながれにみをうらなって)"
      , filename = "hoshinonagareni_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1hc_X37Yq9v-h6BUruUJuVMNo4KzxmtBX"
      , mp3Url = "https://drive.google.com/uc?id=1HvoxTFUG58F_TgvbEgCbuReF9UIwtJXO"
      , title = "一月一日(いちがついちじつ、としのはじめのためしとて)"
      , filename = "ichigatsuichijitsu_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1inlv7kc1MX-dbb8CruQOreyXcVzc5-9E"
      , mp3Url = "https://drive.google.com/uc?id=1ztUG1OikJ7JeqvHFf-UaVnNfMm07B7XQ"
      , title = "かっこう"
      , filename = "kakkou_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1T0IJaCENt_IScNDFTPR2GhN1m5P7Djsx"
      , mp3Url = "https://drive.google.com/uc?id=15BadqA9mHDFaaqeWlOWr13BRkpgdjGRe"
      , title = "きらきら星(きらきらぼし)"
      , filename = "kirakira_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1RrWKKGSWTQ67a-umksQptHwQ-h9zHWZQ"
      , mp3Url = "https://drive.google.com/uc?id=1AQXeEqQyRw_IEDCC6fDz-Mo74NcdblLN"
      , title = "こいのぼり(やねよりたかい)"
      , filename = "koinobori_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1NyUcLay6k1sZ_vLbK-XvQYN3ceZv7CBU"
      , mp3Url = "https://drive.google.com/uc?id=11AWOtUPoD3lo9-4_sHG65dILeHb-Xonv"
      , title = "この道(このみちはいつかきたみち)"
      , filename = "konomichi_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=18Boa9DB6ZxxJsSEb2sXDsv6eCnZ2paNa"
      , mp3Url = "https://drive.google.com/uc?id=1dYYIxaHwSF6t4uxZuuT--0INe9A-WC7h"
      , title = "この世の花(このよのはな。あかくさくはなあおいはな)"
      , filename = "konoyonohana_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1ttMc_D4ZBvm2EzGV_Yh92TrM_-YnMlBY"
      , mp3Url = "https://drive.google.com/uc?id=1vDroijqShW2tf4bzOc72Zg-XbcvpsgU2"
      , title = "ミッキーマウス・マーチ(ぼくらのくらぶのりーだーは)"
      , filename = "mickeymousemarch_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1AA7Jz2e8ta_d_3sxPUeCER9d_NASCXop"
      , mp3Url = "https://drive.google.com/uc?id=1RbdbOoBqc0KBPtph1k1qnE7zG4RCC1OT"
      , title = "みかんの花咲く丘(みかんのはながさいている)"
      , filename = "mikan_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1tpGhYrxnEuVbluuK5rtj0e5RSHatBjPW"
      , mp3Url = "https://drive.google.com/uc?id=1KfcECeAU9dim-g8cb-dB391QiqvZl5b_"
      , title = "虫の声(あれまつむしがないている)"
      , filename = "mushinokoe_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1NfvPvMy2blRpQlZz2BZWS7B6411ZjgqX"
      , mp3Url = "https://drive.google.com/uc?id=1vCqmeM_3oHcPXc0TfcsbVKzGogqAPSQy"
      , title = "南国土佐を後にして(なんごくとさをあとにして)"
      , filename = "nangoku_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=18WLkC8bvQ8tdqTtNTu6BAkC_flrEpPOw"
      , mp3Url = "https://drive.google.com/uc?id=138icNGZq0dw73cICR9_X3MmRyWfuWASj"
      , title = "ワンツー・ジェンカ(おおきくくちあけて)"
      , filename = "onetwojenkka_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=14HRCzYdHtrAVcsfhM3oDCDITJswfVJ8g"
      , mp3Url = "https://drive.google.com/uc?id=17m13_3iKfJbNOWkU6urrK3lNo_5jliEb"
      , title = "再会(さいかい。あえなくなってはじめてしった)"
      , filename = "saikai_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1ZScU3cbPdJpUMp0qebWEJxVIt9PnTUSD"
      , mp3Url = "https://drive.google.com/uc?id=1Q_6JEtn6ABPsOtznk1FmTzdokxxqpo3x"
      , title = "さくらさくら"
      , filename = "sakura_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1XpBG5pqJ6sWxY92znQgZi7lQ5x9LpsHk"
      , mp3Url = "https://drive.google.com/uc?id=14CB_MNaO7JTM-xlcuqDqLaBeROSLq7qf"
      , title = "酋長の娘(わたしのらばさん)"
      , filename = "shuchou_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=18RwmTsJvYcsyfmNdLjrZbYaQQZ4DYuJ8"
      , mp3Url = "https://drive.google.com/uc?id=1jxeYCnXQ7CZMpEIrn_OJ-hm9dGtPiND9"
      , title = "小さな世界(ちいさなせかい、It's a small world、せかいじゅうどこだって)"
      , filename = "smallworld_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1QRcYDpmk4Q5tF6K6RdeRef3AMOBQ_ynn"
      , mp3Url = "https://drive.google.com/uc?id=10uRPITMWRgop4qzQS_E5EHuc6Y7-GD0D"
      , title = "竹田の子もりうた(もりもいやがるぼんから)"
      , filename = "takedanokomori_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1C7hQD5HJtt6KJL2CwSS1oYXzR5FArrRf"
      , mp3Url = "https://drive.google.com/uc?id=1S8_3hTk-Ua7GS72oYqTfxEA5DmYxa8UP"
      , title = "赤鼻のトナカイ(Rudolph the Red-Nosed Reindeer、まっかなおはなの。クリスマス)"
      , filename = "tonakai_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1KLxY-W20A0j6FJZWez0RqHX4YCfe07zV"
      , mp3Url = "https://drive.google.com/uc?id=1kFZhlj3Nd5LXC3TOZdcLN-920FOBXCaQ"
      , title = "幻想即興曲(ショパン)"
      , filename = "gensou_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1IvSpfKYPB1TCcvPFX8f_TNgRLIPWgTnA"
      , mp3Url = "https://drive.google.com/uc?id=1n-i9_UBsLWB_HwPxj-E_9mAlRDydV9lt"
      , title = "君が代(きみがよはちよにやちよに)"
      , filename = "kimigayo_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1eS-Wjq7M7YO0M6YnFHp_YLkFHtl_WkkE"
      , mp3Url = "https://drive.google.com/uc?id=1PObnTX-60xqaB6zEg1FKFrwiy33JEE75"
      , title = "シューベルトの子守歌(ねむれねむらははのむねに)"
      , filename = "schubertkomori_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=18v4zRdw8s_1lHQch_rxfALV9o1vJxEHg"
      , mp3Url = "https://drive.google.com/uc?id=1HZQ9MOdHDPdotCMbk_9dMmN4PIAH88xu"
      , title = "シューベルトの野ばら(わらべはみたりのなかのばら)"
      , filename = "schubertnobara_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1cCBWHJs4VwfcroLZHqcvs_QFT7Sg1itX"
      , mp3Url = "https://drive.google.com/uc?id=1qiJokr2ykC7jkHEqugegogZ-3P3ckjP5"
      , title = "野球拳(やきゅうけん。やきゅうするならこういうぐあいにしやしゃんせ)"
      , filename = "yakyuken_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1QjHP7K6huNnm4YV_bEwldkGQWG-uhKr4"
      , mp3Url = "https://drive.google.com/uc?id=1DSaXADmOrcWTTdBdtG2fZOi61_WLxgPm"
      , title = "赤い靴(あかいくつはいてたおんなのこ)"
      , filename = "akaikutsu_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1k6uJ3XGnI6bHnYbDgIaOjLTrotl-43Vl"
      , mp3Url = "https://drive.google.com/uc?id=1OCpdg5cTNQq6CPxfc6NlUvI-dhLioPxD"
      , title = "赤とんぼ(ゆうやけこやけのあかとんぼ)"
      , filename = "akatonbo_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1vZ4ozxta2MXQWAvR5Xfjv2shg9Bx9cvT"
      , mp3Url = "https://drive.google.com/uc?id=1xp5dKQCLGxxdYtI8FmCpxKInmVY2yQGP"
      , title = "亜麻色の髪の乙女(ヴィレッジ・シンガーズ。あまいろのながいかみをかぜが)"
      , filename = "amaironokami_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=122IFqMEMm4EQ1zVfGc-Rk769EKEiePyo"
      , mp3Url = "https://drive.google.com/uc?id=1ZYZMk6TYeIA1XR75jObU7ezwTVAbKh85"
      , title = "あの子はたあれ(あのこはたあれたれでしょね)"
      , filename = "anokowatare_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1tI9xdaJrcDLgwNrdtNZdZ_defitufjqW"
      , mp3Url = "https://drive.google.com/uc?id=1GsuY-qyz2LdWBYf9BCdEh0hXLefNPCmz"
      , title = "ちょうちょう(ちょうちょうちょうちょうなのはにとまれ)"
      , filename = "chouchou_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1MkJlOO2hWU4f1_jQsCVibgXOw8eo8dUf"
      , mp3Url = "https://drive.google.com/uc?id=18h4bIBi_UYOSOwH0dH1puReyCZFYWKj5"
      , title = "カントリー・ロード(かんとりーろーど、このみち)"
      , filename = "countryroad_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1TwZAhgj81NrOO6INVBhAYOfMQY0WcHhn"
      , mp3Url = "https://drive.google.com/uc?id=1F-UY6UZ0gOIiYi57OiBCiD9qXX3jyvP3"
      , title = "どんぐりころころ(どんぐりころころどんぶりこ)"
      , filename = "donguri_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1S0au-li9_8mMbd3Vx6vlhpvLMSmqfF8g"
      , mp3Url = "https://drive.google.com/uc?id=1v4lnAnqEICWxqcBeMfVlUZfqc-lqKqlB"
      , title = "富士山(ふじさん。あたまをくものうえにだし)"
      , filename = "fujisan_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1Gl_oq-eNY55AOhqpwBpKkozFUoyoipQr"
      , mp3Url = "https://drive.google.com/uc?id=1XqCsNXOgOg6Mc1wPGxIN1gI7KPVVoVw8"
      , title = "春が来た(はるがきた)"
      , filename = "harugakita_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=13ol6A2jqZl8TcTd-_JIvYminXrV8fJ64"
      , mp3Url = "https://drive.google.com/uc?id=1xo-mDkeCIKViX5anj8hd-c79M_z6Lu5A"
      , title = "春風(ふけそよそよふけはるかぜよ)"
      , filename = "harukaze_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1NILvHoECAIHE3dNV2U7qrZFDC57rveTE"
      , mp3Url = "https://drive.google.com/uc?id=1laduIm4xO85JmnB0WLAtdDOCy-vzf1OO"
      , title = "春の小川(はるのおがわはさらさらながる)"
      , filename = "harunoogawa_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1N2yp-2PXoRn4UMWb1A4spC8Te_o-nKbn"
      , mp3Url = "https://drive.google.com/uc?id=1yvuyD1w5J-wQieCt_t5T9zNVfgR0fYQJ"
      , title = "椰子の実(やしのみ。なもしらぬとおきしまより)"
      , filename = "yashinomi_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1EW8U886L9jqdG-DmnlFXweQLgxPDQm61"
      , mp3Url = "https://drive.google.com/uc?id=1NvxcOW3XfsGmbw4ASpKZTxPljlo98wSl"
      , title = "イエスタデイ・ワンス・モア(カーペンターズ)"
      , filename = "yesterdayonce_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1rK9xSFpnZ-mVz4BFCVAe4VQUdZacoYcb"
      , mp3Url = "https://drive.google.com/uc?id=1pbUzmGLfj5n23CEx9OanXdy4NUhZ7XCZ"
      , title = "たこのうた(たこたこあがれ)"
      , filename = "takotako.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1vHoatud8GL2tGaGFbuz-fc5FySfZ-uyI"
      , mp3Url = "https://drive.google.com/uc?id=1k1a_-yvWssztDePXzVzWAm-N5Zzt_tdE"
      , title = "チューリップ(さいたさいたちゅーりっぷのはなが)"
      , filename = "tulip.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1Q2KNpLnyiMMWMG5QYKP7adXXGKr0URZu"
      , mp3Url = "https://drive.google.com/uc?id=1jlRhWy7kdIhergO6-cjnb6Th-Uyt-2Fz"
      , title = "鉄腕アトム(そらをこえてららら)"
      , filename = "tetsuwan.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1hzFWYgZcfAnCkl4EYAOAdAsaP2lnghAS"
      , mp3Url = "https://drive.google.com/uc?id=16WJ8vbWNWXSGHYlPXtk7aY3jYSvC2mgN"
      , title = "手をたたきましょう"
      , filename = "tewotata.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1j4Xq9iU_IS4o6RLZIn8EuPlEXG6D1qEW"
      , mp3Url = "https://drive.google.com/uc?id=1XpNvTdHJxoY6kpauEdobLfrtmnhZhuP4"
      , title = "たなばたさま"
      , filename = "tanabatasama.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1LHP_RpWF-hSWb4IUMIXTfuiETn1gtqBo"
      , mp3Url = "https://drive.google.com/uc?id=1X3nfZgOn2rnD-BzTDo60O-mHoPNS7L7D"
      , title = "つき(でたでたつきが)"
      , filename = "tanabatasama.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1oP0q9blYIYqd8iI8qUvLfGflzL_lYQS2"
      , mp3Url = "https://drive.google.com/uc?id=1k_ogeiXqZO1t2j2blp_YXAPXCGhZ-WAT"
      , title = "アイアイ(あいあいあいあいおさるさんだよ)"
      , filename = "aiai.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1x04LCyD7W7PGX-CfFc7nHJQrcz52vKub"
      , mp3Url = "https://drive.google.com/uc?id=1iQ1fIS_kb-gO76rEvZYRzpnHfIvV0JUI"
      , title = "愛国の花(ましろきふじのけだかさを)"
      , filename = "aikokunohana.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1sZLBoet1rQY2KpaEY-oiOTnuDUujnXho"
      , mp3Url = "https://drive.google.com/uc?id=1aCn5JSJsR_FWh3NK9gd9HXOnc_SA-JV7"
      , title = "赤城の子守唄(なくなよしよしねんねしな)"
      , filename = "akaginokomoriuta.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1zOwlo4l0G0j4_7iLBEURm7BxwE2FCWGT"
      , mp3Url = "https://drive.google.com/uc?id=1SQfGOXhNEMp6SgZqQ6kBFgIjXrK6vOhn"
      , title = "暁に祈る(あああのかおであのこえで)"
      , filename = "akatsukiniinoru.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1v9Alp-27jquhozHihi0BWjtnRPU3Pae0"
      , mp3Url = "https://drive.google.com/uc?id=191copT2VibT0H4sWQwIxrOTp1IRqKGbw"
      , title = "アマリリス(みんなできこうたのしいオルゴールを)"
      , filename = "amaryllis.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1OMEmVbQmVeBBnNWB606QmkvR0hdT-r70"
      , mp3Url = "https://drive.google.com/uc?id=1Ickl_2Sh7BefCKRaC8HeKkPixioGiL5O"
      , title = "アルプス一万尺"
      , filename = "alpsichiman.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1PP7CFxRwKtwUhEsDN4HhUB5zHJBNQJ0L"
      , mp3Url = "https://drive.google.com/uc?id=1jZi0sumxmgzh7RtNz4HYwispequell5M"
      , title = "あわてんぼうのサンタクロース(クリスマス)"
      , filename = "awatenbo.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=13MeHhukH6xGmoKamFD4S83lt_3jDQGV0"
      , mp3Url = "https://drive.google.com/uc?id=1GW5ROHCU-UgY1VUNF6hOsNxSFIBBl5Ez"
      , title = "故郷を離るる歌(そののさゆりなでしこかきねのちぐさ)"
      , filename = "kokyowohanaruru.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1PdO8UWaf4jVljLpuAy8aOEUwmBKQa-h5"
      , mp3Url = "https://drive.google.com/uc?id=1hYS0PSgCJPO-v_3KQZrSCqnUYO9_6NCO"
      , title = "高校三年生(あかいゆうひがこうしゃをそめて)"
      , filename = "kokosannensei.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1NTRw5ZV5sabMKj3_HwIafJSg_6UbFj9a"
      , mp3Url = "https://drive.google.com/uc?id=1sFTI-Rj04SqB0NHU-XYmJawAeM0JzV5m"
      , title = "げんこつやまのたぬきさん"
      , filename = "genkotsu.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1ZJw9F-jWm0w_JNT4FFaeCsFB75Zgzv0G"
      , mp3Url = "https://drive.google.com/uc?id=12szTF4rbz_95LXqq_MnipIe4mHH_95OI"
      , title = "ああそれなのに(そらにゃきょうもあどばるん)"
      , filename = "aasorenanoni.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1JYPnxgIPN_bxlCZDiyytkP_r3880bvrD"
      , mp3Url = "https://drive.google.com/uc?id=1K4TQFXgt53K93O0hQARD4Ness4BKWMQL"
      , title = "青い背広で(あおいせびろでこころもかるく)"
      , filename = "aoisebirode.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1pQzWz4mfkxYAiQxflPuNCsPJ510zQp07"
      , mp3Url = "https://drive.google.com/uc?id=1CxVD87Z8zOzDIoahhuYZgKulypMvt8yq"
      , title = "長崎の鐘(こよなくはれたあおぞらをかなしとおもうせつなさよ)"
      , filename = "nagasakinokane.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1U66InKL9npmROU8XUlVTYmJKhuZp3B4q"
      , mp3Url = "https://drive.google.com/uc?id=12TdhLx5SrVq0J2tdkNvbuv6Otf67tevE"
      , title = "長崎物語(あかいはなならまんじゅしゃげ)"
      , filename = "nagasakimonogatari.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1awuxwfziK58bbfoZENpV0AdGs9k9WYjj"
      , mp3Url = "https://drive.google.com/uc?id=12DqY7QpcHmTPmOKR9e2qisWw3nXj3bDW"
      , title = "ないしょ話(ないしょないしょないしょのはなしはあのねのね)"
      , filename = "naishobanashi.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1ZlUnwrNg5XUOAxY1nVaCJy_m7O7lUfw3"
      , mp3Url = "https://drive.google.com/uc?id=1lvvuDcLxS-woaFsoTwnyOeAkuWuk7uMU"
      , title = "とんび(とべとべとんびそらたかく)"
      , filename = "tonbi.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1MsTYw8MLXZiL2SfhofjIsqdbSoA5auqr"
      , mp3Url = "https://drive.google.com/uc?id=1tYmZ2sVI5HI5gKl2tUCE6dpizNWjAmLr"
      , title = "トンコ節(あなたのくれたおびどめの)"
      , filename = "tonkobushi.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=16LQGzN4BDgm-Txa6RGtLbRCHietXqx1y"
      , mp3Url = "https://drive.google.com/uc?id=1XSW09EYOEjffm-PCVPSCzQznw6GqNE2B"
      , title = "隣組(とんとんとんからりととなりぐみ)"
      , filename = "tonarigumi.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1hT4qCwQ9Xmvb4jrjLkw61TkxjhAPTGN_"
      , mp3Url = "https://drive.google.com/uc?id=1Jl1RsrTooii0sgx4_rTzpMmgI1UiKFrF"
      , title = "どこかで春が"
      , filename = "dokokadeharuga.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1UkjEGNtTjuY-U0UdsZE5W8Ac84GJfZo2"
      , mp3Url = "https://drive.google.com/uc?id=1DusYJosZSNMid1R6RRXHXzBPHi0nGWBW"
      , title = "通りゃんせ"
      , filename = "toryanse.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1xrFn9jbVin6nG05-5wR8VBNoBCaQO2xp"
      , mp3Url = "https://drive.google.com/uc?id=1seVQLSfi-ujiWpBW2_dAxNALtGRVCmr0"
      , title = "天国に結ぶ恋(こよいなごりのみかづきも)"
      , filename = "tengokunimusubukoi.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1C5BHdxtJgBx3D3rw6LQxDv6ih6Z5KcYt"
      , mp3Url = "https://drive.google.com/uc?id=1N-RXZ3mlKd-ZjijXqPTNfC6wn3ugLN_e"
      , title = "われは海の子(われはうみのこしらなみの)"
      , filename = "warewaumi.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1E3Ll7wE3YQ8S_Wqj5H-fh1NoxpJV_fZ2"
      , mp3Url = "https://drive.google.com/uc?id=1SWixMVXncKkpZXORY3qqnu0mb0_I29Wq"
      , title = "喜びの歌(はれたるあおぞらただようくもよ)"
      , filename = "yorokobi.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1pujCfxMZVMaYkZAz_4SlqEQLDQzNLWwY"
      , mp3Url = "https://drive.google.com/uc?id=1Cb04QLvT3UvuhX8MhABmyNAIZ7Vp_nD2"
      , title = "夢路より(ゆめじよりかえりてほしのひかりあおげや)"
      , filename = "yumejiyori.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1pHUnNWGV5wjBc4jmfCQmga5hZNmdlxcr"
      , mp3Url = "https://drive.google.com/uc?id=10DniZHZ3-IPLTOgZwIpBZr5B1P78ApPY"
      , title = "湯の町エレジー(いずのやまやまつきあわく)"
      , filename = "yunomachieleby.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1qVX7IPcTVTOrnNrKEf90ZeMAKq0b9MaZ"
      , mp3Url = "https://drive.google.com/uc?id=19GeCGxKLE6evrE0z0YLyK3o0zQK1A0iC"
      , title = "雪山讃歌(ゆきよいわよわれらがやどり)"
      , filename = "yukiyamasanka.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=183GpC0-bxi3hqEuITo5DvZDjzIIl4WzN"
      , mp3Url = "https://drive.google.com/uc?id=1cYBC8QH3h_4Mg7lVlbZADiLQX374oCuU"
      , title = "夕日(ぎんぎんぎらぎらゆうひがしずむ)"
      , filename = "yuhi.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1WEKzSiq-UNkRsxVRxe5x7x_L9bSPZ7N5"
      , mp3Url = "https://drive.google.com/uc?id=1M8VV8Z-TiMJ7dp1vbzxH28f488ghabs1"
      , title = "東京音頭(とうきょうおんど。はあーおどりおどるならちょいと)"
      , filename = "tokyoondo.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1Qa-4hVWdLYZz_2LM40ej8iLj5gQrRkc2"
      , mp3Url = "https://drive.google.com/uc?id=18DUNRRaJNYLqn5_kiPC4S5V_25uYMcdO"
      , title = "ローレライ(なじかはしらねどこころわびて)"
      , filename = "lorelei.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1S_KO2NsVQnUtwykXph6T5-LBKrbUfz6Q"
      , mp3Url = "https://drive.google.com/uc?id=1wnsrGB4hpE1lrxJgtpGve6lQVy6osVZm"
      , title = "露営の歌(かってくるぞといさましく)"
      , filename = "roeinouta.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1MeMTWmbSMO59jgPDmNNPzSuIYnGptIBW"
      , mp3Url = "https://drive.google.com/uc?id=1ty_VQhVCQMT1b0EF_CenqsEPT0ZLWNXG"
      , title = "リンゴのひとりごと(わたしはまっかなりんごです)"
      , filename = "ringonohitorigoto.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1hfAydttdTrvhwG9hzAPtsetwWJlFzT5R"
      , mp3Url = "https://drive.google.com/uc?id=1tJjKuEk2qGQcCdnEvanI7ZKC85SB-tc4"
      , title = "リンゴの歌(あかいりんごにくちびるよせて)"
      , filename = "ringonouta.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1KO1lpvy2FdbZbUDjpXlby2tAXaeLBMmt"
      , mp3Url = "https://drive.google.com/uc?id=1BRU-xzQU5tjz41rWMpcKYF1gzFrIBv6r"
      , title = "旅愁(ふけゆくあきのよたびのそらの)"
      , filename = "ryoshu.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=16tauHVEefEK8Jqbc-g1A-lkWB1hsLyQF"
      , mp3Url = "https://drive.google.com/uc?id=1FygfukFnx8hG7pC21c_-s7DILxSJJlkm"
      , title = "柔(かつとおもうなおもえばまけよ)"
      , filename = "yawara.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1UZbd4XAhTD5DWrT_LpMNasXX3iZRXYcy"
      , mp3Url = "https://drive.google.com/uc?id=1Abq4PrC_o7O5t7s_r9npCENWXOlEEBHO"
      , title = "森の水車(みどりのもりのかなたから)"
      , filename = "morinosuisha.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1-WJ1AN-4PyTAt6q5mlWuSsgq1venZJwh"
      , mp3Url = "https://drive.google.com/uc?id=1ZwAdHJKCD98e9XvLCP7LtTYCa5cfSvRq"
      , title = "桃太郎(ももたろさんももたろさんおこしにつけたきびだんご)"
      , filename = "momotaro.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=13QuxkMVg1gDLKGGRMHJ4z-cvBWLeVU0g"
      , mp3Url = "https://drive.google.com/uc?id=1HpWU1ALfTSgFY-j6L6q5UbK2fU7Lqs0v"
      , title = "村の鍛冶屋(しばしもやすまずつちうつひびき)"
      , filename = "muranokajiya.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1QD03rIqSg_7TY_NXX0GdmfLUYD7ZgwKo"
      , mp3Url = "https://drive.google.com/uc?id=1oui6DyXrqYWAMrvSL1SeHXVTEsP_s8OZ"
      , title = "むすんでひらいて(むすんでひらいててをうってむすんで)"
      , filename = "musunde.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1RfceWQRNBrLtbUQ1fmRimF1Y0vz1lSiD"
      , mp3Url = "https://drive.google.com/uc?id=1wXhWbAbGfd0UxSKPcZa87-bM-lt-bUga"
      , title = "仰げば尊し(あおげばとうとしわがしのおん)"
      , filename = "aogeba.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1bA3q21dNRaqtc8LY7BjmkGE9I3j6xLX6"
      , mp3Url = "https://drive.google.com/uc?id=1mXIa3E4212g41CWLdoveCZ7yhMc8i_UM"
      , title = "あの町この町(あのまちこのまちひがくれる)"
      , filename = "anomachi.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1hp_gGB3M228_JcyOm15zgoaXXWxmBEfA"
      , mp3Url = "https://drive.google.com/uc?id=1hdy6zjR0_VlMS6EejWcXQjHPgau89oAX"
      , title = "雨(あめがふりますあめがふる)"
      , filename = "ame.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=10zAOp0w-5JZMEbUanYHw1VrWVu_7Wz-A"
      , mp3Url = "https://drive.google.com/uc?id=1bCcxaVcVFVf5LknsTh-EjM2zt3n4Tdlu"
      , title = "一寸法師(ゆびにたりないいっすんぼうし)"
      , filename = "issunboshi.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1tGiA9HoEqIR-oCC14mAppe1wlPJSRd5x"
      , mp3Url = "https://drive.google.com/uc?id=1An1JMg3PBChR5oh_x6YeWfQ35kw6_50y"
      , title = "うさぎ(うさぎうさぎなにみてはねる)"
      , filename = "usagi.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1hPec2PBNt7JOfUQeC-2D4oj7xalvbMX-"
      , mp3Url = "https://drive.google.com/uc?id=1W0FziUExLRv5uZAQx0mkSyCQNS4j8Chf"
      , title = "兎のダンス(タラッタラッタラッタ)"
      , filename = "usaginodance.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1N7v9vUBjneGOBkYCld1loJLpJm-4Tvy7"
      , mp3Url = "https://drive.google.com/uc?id=1EYX9vEimFxmheBcfSW6yYeKeWn3wr7tF"
      , title = "別れ船(なごりつきないはてしない)"
      , filename = "wakarebune.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1KIpmWnqaydeM4DJvBzNwCrxVeptIJOnZ"
      , mp3Url = "https://drive.google.com/uc?id=1ukG2ddTIjNxixk3wFcxuHa6LoaERkOZ9"
      , title = "水色のワルツ(きみにあううれしさの)"
      , filename = "mizuironowaltz.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1H8-GLzC9Q_HPfPuP48xLLbmU_EQtMXcd"
      , mp3Url = "https://drive.google.com/uc?id=1I67md4E3xmihAW4NUtkpxMSBhuFFiwSj"
      , title = "啼くな小鳩よ(なくなこばとよこころのつまよ)"
      , filename = "nakunakobato.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=15cr3drUT2jqko4x9EFIcAydXJHX78bv9"
      , mp3Url = "https://drive.google.com/uc?id=1KPPux4R0iWYFLmgXkT3nlp7S0ARO4gsm"
      , title = "人形(わたしのにんぎょうはよいにんぎょう)"
      , filename = "ningyou.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=12KlKLNy7qB5zYJpw_h-HfSsK_D1BoIAc"
      , mp3Url = "https://drive.google.com/uc?id=1-EnDUJAs3H2mXxAuQUNTThOwEqwr2aeZ"
      , title = "おうま(おうまのおやこはなかよしこよし)"
      , filename = "ouma.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1nCI0J-uQi3JjOF8r-6cAlbdrtLe6IuOg"
      , mp3Url = "https://drive.google.com/uc?id=1fJlzuVAvv8HlLFnfEAZAYFKlzrcCrm_w"
      , title = "お江戸日本橋(おえどにほんばしななつだち)"
      , filename = "oedonihonbashi.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1kJ4b3-OR4Ss4H2kTDFFuvvs_7pOa7f9n"
      , mp3Url = "https://drive.google.com/uc?id=1zGxhzgyQzCioWc2q-ndPsngTc31B62Wl"
      , title = "男の純情(おとこいのちのじゅんじょうは)"
      , filename = "otokonojunjo.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1VvglMG-PgfxVGFVSdbItP5YT2Yk8S7N-"
      , mp3Url = "https://drive.google.com/uc?id=1zYbflXUGPh9uanDKb0fUtFgBKess3CED"
      , title = "籠の鳥(あいたさみたさにこわさをわすれ)"
      , filename = "kagonotori.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1usM9Vb2MzX23QXM9TgS2MM-koleuoqQ2"
      , mp3Url = "https://drive.google.com/uc?id=1W3TCfx2EcpHzwAZoTjEx8-Pd_9VUg7BO"
      , title = "霞か雲か(かすみかくもか)"
      , filename = "kasumikakumoka.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1o1OpPnfjETSYry01NqLgs-KFQ3xXl6T7"
      , mp3Url = "https://drive.google.com/uc?id=1htBCrCocgK3yEJUEMIcuiPw80Y0vj6NJ"
      , title = "愛国行進曲(みよとうかいのそらあけて)"
      , filename = "aikokukoushinkyoku.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=11cN2dGIaIQ5ettwV6w__RjUdgOvgnGaA"
      , mp3Url = "https://drive.google.com/uc?id=1tfd7nMuibpLTtRt16BVpk_RHjXoBtd_s"
      , title = "かなりや(うたをわすれたかなりやは)"
      , filename = "canary.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1FKW6HVsOWC13OeY-wQFoBO52eNV6P0Mk"
      , mp3Url = "https://drive.google.com/uc?id=1aN36sgqSVuMOc1RQJsjMtzs6B2xmFVmE"
      , title = "鎌倉(しちりがはまのいそづたい)"
      , filename = "kamakura.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1zq8n5GytjTfsVNOhZR9ElRdTN5vq4M6e"
      , mp3Url = "https://drive.google.com/uc?id=1e3PqIRk1uLk0ykS9S2htaLznGfnIKDrU"
      , title = "祇園小唄(つきはおぼろにひがしやま)"
      , filename = "gionkouta.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1sNMDchceP9jQIShFLTEIOJFBNW7yrmsn"
      , mp3Url = "https://drive.google.com/uc?id=1Ap1Ujj2e6h37hCgCMO9C32j5_xFjv2z_"
      , title = "こうま(はいしいはいしいあゆめよこうま)"
      , filename = "kouma.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1dtUw5I9-JKGwjpnLe4lYKtMpY4wneB3H"
      , mp3Url = "https://drive.google.com/uc?id=1RZD7C6eRljOnZJDszAw2kNW4h6z9XjCO"
      , title = "お富さん(いきなくろべいみこしのまつに)"
      , filename = "otomisan.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1Cp87RUb0_YPHyj0peMqrJk3TWaUEQzbz"
      , mp3Url = "https://drive.google.com/uc?id=1atDmXTGnCXFjeqIwUdFMAZoloO1icviB"
      , title = "いい日旅立ち(ゆきどけまじかの)"
      , filename = "iihitabidachi.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1U8KS0x8Ocura_eZkJ0d2d0s0sJZgSEVK"
      , mp3Url = "https://drive.google.com/uc?id=1sCNJpF5SyRto08gQGy_6aW2-zpK03G_G"
      , title = "アラビアの唄(さばくにひがおちて)"
      , filename = "arabianouta.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1SPUkVZ6_VJgjr4q1lJEFwoFzF_p6zJj6"
      , mp3Url = "https://drive.google.com/uc?id=1LpspAcK4dttEfSF4zNXX_uzVzK-wsFpV"
      , title = "ハバロフスク小唄"
      , filename = "khabarovsk.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1EAqRgKfmM0e8tS4LkMP5VqoFl5YzHFBe"
      , mp3Url = "https://drive.google.com/uc?id=1qqpxzmEDIL_Vp1JPBpekRNIiupQvLXNl"
      , title = "人を恋うる歌(つまをめとらばさいたけて)"
      , filename = "hitowokouruuta.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1ClwkIMpH2qBfCwfeVIMqg-OvDI7hdDQT"
      , mp3Url = "https://drive.google.com/uc?id=1yLyidhYX6dVZz-XpRvv8983bAL0dxOrZ"
      , title = "蛍の光(ほたるのひかりまどのゆき)"
      , filename = "hotarunohikari.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1plrEKN8Y5UpH2eBuUktHxC90RKtO40Wd"
      , mp3Url = "https://drive.google.com/uc?id=1qQayn5QyVOgbhaKz--S9-sq0eOuDmVlb"
      , title = "麦と兵隊(じょしゅうじょしゅうとじんばはすすむ)"
      , filename = "mugitoheitai.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1dpX8iyiBkPMtU-NVfy-nR6B19kokuhE-"
      , mp3Url = "https://drive.google.com/uc?id=1BGF7fh9GuirKEX7ivBBLAiVv2zN9HYZH"
      , title = "たばこやの娘(むこうよこちょうのたばこやの)"
      , filename = "tabakoya.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1iWre5qvGBvkhXLZWUfL4v-eSDcKnSWk9"
      , mp3Url = "https://drive.google.com/uc?id=10CmpPNP-U7Ui4UMuKpIj_SejtZolx3fz"
      , title = "戦友(ここはおくにをなんびゃくり)"
      , filename = "senyu.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1UrQznxPpZ9kUg5vEefe3dCDizDdyDD2D"
      , mp3Url = "https://drive.google.com/uc?id=1CmtL7-YmNxGsdCjD__Oay9o3S01Mxu9X"
      , title = "聖夜(きよしこのよる)"
      , filename = "kiyoshi.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1do7NA3n64U9sFnYVlUZzTfnT_LDxu6RQ"
      , mp3Url = "https://drive.google.com/uc?id=1zPpRO5xKwJdogi-ayU2PJGTi1_PmceVf"
      , title = "水師営の会見(りょじゅんかいじょうやくなりて)"
      , filename = "suishiei.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1icO60KbahCHOqYgTzw8zwlK_UVp5PWm4"
      , mp3Url = "https://drive.google.com/uc?id=1ATE2aqVZfk_p9MdqjlLsmVY-g73aa-y8"
      , title = "ああモンテンルパの夜は更けて(モンテンルパの夜は更けて。Muntinlupa, フィリピン)"
      , filename = "muntinlupa.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1bfgHqSUyLOA3qWtoxOVv_6yCn1_ZOfmw"
      , mp3Url = "https://drive.google.com/uc?id=1bQ9x8vhGDTvSKQlJOfMHB-fSleB6aEGa"
      , title = "君の名は(きみのなはとたずねしひとあり)"
      , filename = "kiminonawa.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1qCclnMVCVXqlnisSweNSRDXRH7Rr9d7G"
      , mp3Url = "https://drive.google.com/uc?id=195AS_p7wHipTgNWM39lRWm24US5uunhD"
      , title = "燦めく星座(おとこじゅんじょうのあいのほしのいろ)"
      , filename = "kirameku.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1sXkqylmP0GKWctzx8CO69GJLp8HB3oMb"
      , mp3Url = "https://drive.google.com/uc?id=1-DNvDUoLpfrMbM4KriXxiOfzHtU7ABam"
      , title = "宵待草(まてどくらせどこぬひとを)"
      , filename = "yoimachigusa.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1rNqYGsaFWlP5lRNuE1Qi19Y09nxZjQBb"
      , mp3Url = "https://drive.google.com/uc?id=1pTCmB9DTSI296H62MVIvV0XsHJSJD-dn"
      , title = "懐かしのブルース(ふるいにっきのぺーじには)"
      , filename = "natsukashinoblues.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1wvU2JWhVd6tLlPT48dE-HFLLh1CnV3GV"
      , mp3Url = "https://drive.google.com/uc?id=1Cb3BnafN8_UZQ9-TTjnCmPQquDIVRCE8"
      , title = "悲しき口笛(おかのほてるのあかいひも)"
      , filename = "kanashikikuchibue.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1TtXPS75SZqz3dkCIHobOD4bLR3SsdXCy"
      , mp3Url = "https://drive.google.com/uc?id=14xcC1aCDj9U5iZGjjtMHfmZhiOXUVsJs"
      , title = "ジングル・ベル(クリスマス。のをこえておかをこえ)"
      , filename = "jinglebell.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1HlhevszRm1pJl8UH-IlIvgFKxBDDXuQB"
      , mp3Url = "https://drive.google.com/uc?id=17lJ_XLyiXrrQfcEgrWm06YSuT9QCEPFt"
      , title = "勇気100パーセント(がっかりしてめそめそしてどうしたんだい)"
      , filename = "yuki100p.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1sfJUZjg2MESyNLjq7PLbO6dicXMJK-kw"
      , mp3Url = "https://drive.google.com/uc?id=1BZXK0i7BXIIiHaobvKnSNmTz6WFilB5i"
      , title = "好きだった(すきだったうそじゃなかったすきだった)"
      , filename = "sukidatta.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1zyDxoe-DpBTbVDM-EB-d6LeY13G1u4xn"
      , mp3Url = "https://drive.google.com/uc?id=1AxMqUgTfL4NyVsBdLA-FY89c8EL11NJA"
      , title = "四季の歌(はるをあいするひとは)"
      , filename = "shikinouta.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1Oph1R4qvAVvvphHxlZJ4Rpob-ZBnzUpx"
      , mp3Url = "https://drive.google.com/uc?id=18t43GAZWMFEtBX1zaK-Fn1e5sB1ZLrlZ"
      , title = "仲よし小道(なかよしこみちはどこのみち)"
      , filename = "nakayoshikomichi.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1DoTSkiCA5-qhehIS1_9fwwQwK2k3U1ZC"
      , mp3Url = "https://drive.google.com/uc?id=1YXchgECNKskPYUTPNQtNopALvcSNJ5k8"
      , title = "ドレミの歌(どはどーなつのど)"
      , filename = "doreminouta.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1EwSgq9rXef_aPn-KNjJR8qw__mz1f2Wl"
      , mp3Url = "https://drive.google.com/uc?id=1nNbubE84_wM-BO4l3C6OQgbBqbcBFY8E"
      , title = "案山子(やまだのなかのいっぽんあしの)"
      , filename = "kakashi.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1D_5y_wxLYQeVc7oq_O4nkyLK9DTGaEeR"
      , mp3Url = "https://drive.google.com/uc?id=1Mhwb0IcHhUI376V8PcuqjcknaIBmB_8F"
      , title = "影を慕いて(まぼろしのかげをしたいて)"
      , filename = "kagewoshitaite.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1bf265Ya1sYMbR3Fk8WNJfKHO3Ujlm0ol"
      , mp3Url = "https://drive.google.com/uc?id=10AAQ6JoGnOZKrO_RZhoCGmjy6VYzxd4x"
      , title = "鞠と殿さま(てんてんてんまりてんてまり)"
      , filename = "maritotonosama.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1-Yk5FPtggwtSJwLGOfOo66OUhIYTT4gY"
      , mp3Url = "https://drive.google.com/uc?id=1ylKBBuGZmq1Q2XKPLB5H89KvHkByIuqM"
      , title = "真白き富士の嶺(七里ヶ浜の哀歌。ましろきふじのね)"
      , filename = "mashiroki.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1DsROegZvXPMpuqkr28ptuAjgpz7Mtaf_"
      , mp3Url = "https://drive.google.com/uc?id=1Po48fBznv9MewAAc89pr01muJdAAHhco"
      , title = "学生時代(つたのからまるちゃぺるで)"
      , filename = "gakuseijidai.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1Luq-ywo6fVTn-PAx0P8Fbm5vW8QIlHcD"
      , mp3Url = "https://drive.google.com/uc?id=1cAUMJMb9AjAjnzEZt5k9LKM9P1IifMMv"
      , title = "三百六十五歩のマーチ(しあわせはあるいてこない)"
      , filename = "sanbyaku65.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=172CKkz1-8tDByGfHyg0ZH9CM3TN0m8hi"
      , mp3Url = "https://drive.google.com/uc?id=1IXzWrvyJIyKLtnMID0kWmtNjY21VOVoa"
      , title = "別れのブルース(まどをあければ)"
      , filename = "wakarenoblues.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=14C6WWx8k8OYSs6UU244wgbXCKvXxtjx2"
      , mp3Url = "https://drive.google.com/uc?id=1gx8EPfjmsskIsDBp_WCiIIlZ5gjJG6FK"
      , title = "桑港のチャイナタウン(さんふらんしすこのちゃいなたうん)"
      , filename = "chinatown.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1tPI-ovIXxvbVma7dRDETM4YPRToU_qOh"
      , mp3Url = "https://drive.google.com/uc?id=11m0ZthymUS3WE3sdegBtjIEKETGt-AgA"
      , title = "さんぽ(あるこうあるこうわたしはげんき)"
      , filename = "sanpo.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=13-I4Y5-EOrZ3asCKG_u-9gjqqys2RJjq"
      , mp3Url = "https://drive.google.com/uc?id=1xL83PTGp8RRd0OCwcRFK52iU-Bv8PVnW"
      , title = "森の小人(もりのこかげでどんじゃらほい)"
      , filename = "morinokobito.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1t2Wv1nPtZ0r9qe1QPKGFvZStGksy-gcU"
      , mp3Url = "https://drive.google.com/uc?id=14NVcygyC4bHevi3L6RrYghDV9z4qI-at"
      , title = "若鷲の歌(わかいちしおのよかれんの)"
      , filename = "wakawashinouta.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1jYMPozc7GJjBxl-Rwryy1GsH1Vld-9ES"
      , mp3Url = "https://drive.google.com/uc?id=1iHY9QxXOt0xTBjP3aIjMI_O9nSqOegKs"
      , title = "冬景色(さぎりきゆるみなとえの)"
      , filename = "fuyugeshiki.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1Cvhc7m2Nk246-YQaRpFOgQYLf-CyTnDC"
      , mp3Url = "https://drive.google.com/uc?id=1N3REHPL99O7qyBHhov3xFX8VctUWOKhi"
      , title = "翼をください(いまわたしのねがいごとがかなうならば)"
      , filename = "tsubasa.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1zEpwec3XOmAqdqneKfnWAHXWmeDrDa-1"
      , mp3Url = "https://drive.google.com/uc?id=1bqQLoa4Fb4YeRTJNRH96djPd5BPP0nnC"
      , title = "青い目の人形(あおいめをしたおにんぎょは)"
      , filename = "aoimewoshita_crop.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=16LMZSt3VBo-Q1czxD4Tg16Qpj2ew2_7b"
      , mp3Url = "https://drive.google.com/uc?id=14oMTiUp9Rga9OG-pNB9UogM4wpNHq1L6"
      , title = "もみじ(あきのゆうひにてるやま)"
      , filename = "momiji.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1mP8wdlTbXjXzwyY4zH2IQYUkJDw-CnUI"
      , mp3Url = "https://drive.google.com/uc?id=1X00S4Op49l2S3WozFl2-t9LqZ_gwCV_G"
      , title = "美しき天然(そらにさえずるとりのこえ)"
      , filename = "utsukushikitennen.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1DxCjI9fGPli_2U6MnAz1Fgh4v3fKAthC"
      , mp3Url = "https://drive.google.com/uc?id=1XKERMhIY_LOgW8v3b5AYOJRKqd4FECsM"
      , title = "青い山脈(わかくあかるいうたごえに)"
      , filename = "aoisanmyaku.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1srLJLOFrmkGWqDebdaICn5Efjjw-G5BT"
      , mp3Url = "https://drive.google.com/uc?id=1ty_eWfUlyCkYe0OEAngxtI-Fl49ab9Bv"
      , title = "上を向いて歩こう(うえをむいてあるこう)"
      , filename = "uewomuite.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1kcL4DhyqlYQfS7eM1KbUmyZIaQQfZ4nw"
      , mp3Url = "https://drive.google.com/uc?id=1zcm6wqdWmkp2Rpuun61ZE4SOJPCbn6wf"
      , title = "ふるさと(うさぎおいしかのやま)"
      , filename = "furusato.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1543rLp3RFm3Ih3UpO83UpgaIZe1QC1R4"
      , mp3Url = "https://drive.google.com/uc?id=1tdgfvcYJbYfPD1HmcnK4DIKxfZ1N-6Js"
      , title = "うつくしき(うつくしきわがこやいずこ)"
      , filename = "utsukushikiwagako.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1x5bSOD6ImLfX9AeUfw_RQQiY0JJP2S0L"
      , mp3Url = "https://drive.google.com/uc?id=1x1oZX26krFxvopVrc85HV_9i3RQdmYvD"
      , title = "浜辺の歌(あしたはまべをさまよえば)"
      , filename = "hamabe.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1EWvGQAdTt7qi2iQGIqrIqDRgO-6HGbUP"
      , mp3Url = "https://drive.google.com/uc?id=1YJWJN3qfo-M8oAg3kGEzJlm2XR4Kgzep"
      , title = "歌の町（よい子がすんでるよいまちは）"
      , filename = "utanomachi.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=16mJlcIdAOTn856x2VNCp5LkWybzjeeFf"
      , mp3Url = "https://drive.google.com/uc?id=1j-w1g6Uthw3el-2xvI3PPjf2Q7UAa-nU"
      , title = "叱られて(しかられてあのこはまちまでおつかいに)"
      , filename = "shikararete.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1kbATpiDtifPkyCLV_of33XCT1IrJF0ZV"
      , mp3Url = "https://drive.google.com/uc?id=1xiSAG-qNSQ57Uf5LGZ5FpaUXT24nbu8i"
      , title = "少年時代(なつがすぎかぜあざみ)"
      , filename = "shonenjidai.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1Etn8jnoBUBysLuQL3JjrPeqOi8A-oz3L"
      , mp3Url = "https://drive.google.com/uc?id=1DPaewK8L4fUTAgO27LSZU74LdCVVy0SE"
      , title = "ペチカ(ゆきのふるよはたのしいぺちか)"
      , filename = "pechka.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=15GFgLIcsMuXpvrXFeke_-5n_sGuhgyj3"
      , mp3Url = "https://drive.google.com/uc?id=1Zk-mEbKwkAkS1Mv3739Fp39BkyjNNWW8"
      , title = "ハイ・ホー(くちぶえをげんきにふきならし、ディズニー「白雪姫」)"
      , filename = "heighho.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1yNb_hgcUcS2yhL6y-C0IR3RkP14gw5zH"
      , mp3Url = "https://drive.google.com/uc?id=1SKoTRAQEq_AJIMifawTAkLjsuNW3qODE"
      , title = "日の丸の旗（しろじにあかくひのまるそめて）"
      , filename = "hinomaru.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=12r_phKOpMKpuuONT-yKQ-sUF7ga8wE9g"
      , mp3Url = "https://drive.google.com/uc?id=1ZQfodN3gM6BZuzSPBgK6kaZj_GJVTRa7"
      , title = "ここに幸あり(あらしもふけばあめもふる)"
      , filename = "kokonisachi.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1IHQftIQnW50VznxYsMYMFjFhIalkkufu"
      , mp3Url = "https://drive.google.com/uc?id=1GJMpEGadPsX0RXVdRRy89_kv_GGv-NDq"
      , title = "船頭さん(むらのわたしのせんどさんは)"
      , filename = "sendo.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=11P1bRPGgRe2jpHfyp8hf9YyzDFOhG_Uq"
      , mp3Url = "https://drive.google.com/uc?id=1pBWatBDUqpYCTsYn1QZYN3x8kfaCE7So"
      , title = "すみだ川(いちょうがえしにくろじゅすかけて)"
      , filename = "sumidagawa.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1xkwyezJS-y8bPioL7ETN1QKcxD56h_sK"
      , mp3Url = "https://drive.google.com/uc?id=1oX8_axx6ICXtSqQANokK8Q9ZUKhtU8u-"
      , title = "誰か故郷を想わざる(はなつむのべにひはおちて)"
      , filename = "darekakokyowo.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1EpLVJ7bzYqjPltVv6onWcPOSWS4qkXXs"
      , mp3Url = "https://drive.google.com/uc?id=1hVZr4Wy-Lu-D37R7NlW0-2lDPORKqCt-"
      , title = "人生劇場(やるとおもえばどこまでやるさ)"
      , filename = "jinseigekijo.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1iz8929aoeRwTZvK5krR8BXqwmiPRXB69"
      , mp3Url = "https://drive.google.com/uc?id=1liG2KJmpcKPVtNZmsQ2Ad-ItMq9JgQZK"
      , title = "若いお巡りさん(もしもしべんちでささやくおふたりさん)"
      , filename = "wakaiomawari.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1_Nrw6fjI7kzGwfu_kWvlYdn8kETyvc0B"
      , mp3Url = "https://drive.google.com/uc?id=1LM1PubosJqRlZ-pfOmGQDlf4JQU5n0X3"
      , title = "冬の夜(ともしびちかくきぬぬうははは)"
      , filename = "fuyunoyoru.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1g6G2B8XsqxI08M9tPC05paFEz4G7s-J1"
      , mp3Url = "https://drive.google.com/uc?id=18AaVTO-KhW8IZz1egNzu1JoWOijt3liQ"
      , title = "軍艦マーチ(まもるもせむるも)"
      , filename = "gunkan.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1uQUJJn-PM0HgyRuhoLylhQ927uIZA6A6"
      , mp3Url = "https://drive.google.com/uc?id=1KS4aY0fLc-zgijI_uiXUyJZXbEJ_oiT4"
      , title = "カチューシャ(りんごのはなほころび)"
      , filename = "katyusha.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1MsEND9XGBH660ijEQ9r5A6l7Bw_GWqOu"
      , mp3Url = "https://drive.google.com/uc?id=1qgAEpjx8F1EmmWmIoItpt0jNK0bRgR7l"
      , title = "世界に一つだけの花(はなやのみせさきにならんだ)"
      , filename = "sekainihitotsu.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1q8O5il6q_k2L05yZbbPdyWtWHjc8-lcE"
      , mp3Url = "https://drive.google.com/uc?id=1RaXRO1Wm15RgDb4-SzFcqV_SfzpkI9Ys"
      , title = "春よ来い(はるよこいはやくこいあるきはじめた)"
      , filename = "haruyokoishoka.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1jY7CaJu4yIUqPHxcjFr9k4UtbakPnxRq"
      , mp3Url = "https://drive.google.com/uc?id=1JKMKdaea6SO5odqucNZhiTd4uKaxQVG1"
      , title = "トロイカ(ゆきのしらかばなみき)"
      , filename = "troika.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=17WuP5n3y6n-5iqswA_PBWmvQfKOZC0_L"
      , mp3Url = "https://drive.google.com/uc?id=11YlAGKXT1eChGaj-C2GCAC8eVeAwlaZZ"
      , title = "同期の桜(きさまとおれとは)"
      , filename = "doukinosakura.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1QsuVPLF3KES2r4HrlPek-tiHUsXdBwgv"
      , mp3Url = "https://drive.google.com/uc?id=1Nr2zhDDDZf3PqgLcn5OW5q0hhx5XRR-q"
      , title = "一週間(にちようびにいちばにでかけ)"
      , filename = "isshukan.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1elUHV6YB0WZBuUFpzztZHYqAoCr4lLdL"
      , mp3Url = "https://drive.google.com/uc?id=1sb3nxFRAx9PyEHg1ZmGgtu6Ylna3gX4D"
      , title = "川の流れのように(しらずしらずあるいてきた)"
      , filename = "kawanonagare.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1Y562qGRvd-9EtzTxCln2lJ3JibHBvtSo"
      , mp3Url = "https://drive.google.com/uc?id=1tU1Pwv5ZNx9HMF05se3s5q706FIUqosa"
      , title = "夏の思い出(なつがくればおもいだす)"
      , filename = "natsunoomoide.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1X6rWwnDJeB0XnPa7uIELgfJuYMpOAJEP"
      , mp3Url = "https://drive.google.com/uc?id=104FL3RcK0qNFSuB47sc0Gy27UHjm9hIP"
      , title = "線路は続くよどこまでも(せんろはつづくよどこまでも)"
      , filename = "senrowa.pdf"
      }
    , { jpgUrl = "https://drive.google.com/uc?id=1rZpgU-oY9YENvOM0EIgBiPPetB637lIC"
      , mp3Url = "https://drive.google.com/uc?id=1-pa2-bO4FXGjr1XtN5vkeN044LGZPf4t"
      , title = "幸せなら手をたたこう(しあわせならてをたたこう)"
      , filename = "shiawasenara.pdf"
      }
    ]



-- , { jpgUrl = ""
--   , mp3Url = ""
--   , title = ""
--   , filename = ".pdf"
--   }
