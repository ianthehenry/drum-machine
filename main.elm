module Drummer where
import Graphics.Input as Input
import Array as Array
import Array (Array)
import Native.Error
import Http
import Json
import Dict as Dict
import Dict (Dict)

data Sound = Node Bool | Pair Sound Sound

isNode : Sound -> Bool
isNode sound = case sound of
    Node _ -> True
    _ -> False

data Direction = Left | Right
type Path = [Direction]
type Instrument = String

data Command
    = Flip Instrument Path
    | Join Instrument Path
    | Split Instrument Path
    | Noop

type State = Dict Instrument Sound

commands : Input.Input Command
commands = Input.input Noop

mapD : (comparable -> b -> c) -> Dict comparable b -> [c]
mapD f dict = map (uncurry f) (Dict.toList dict)

serialize : State -> Json.Value
serialize state =
    let jsonifySound sound = case sound of
        Node a -> Json.Number (if a then 1 else 0)
        Pair l r -> Json.Array (map jsonifySound [l, r])
    in Json.Object (Dict.map jsonifySound state)

button : Color -> Command -> Int -> Int -> Element
button background command w h =
    let btn alpha = layers [ color background (spacer (w - 1) (h - 1))
                           , color (rgba 0 0 0 alpha) (spacer w h)
                           ]
    in Input.customButton commands.handle command (btn 0) (btn 0.1) (btn 0.2)

circleE : Color -> Float -> Element
circleE c r = let d = (2 * r) in
  collage d d [filled c (circle r)]

textButton : Command -> (Int, Int) -> String -> Element
textButton command (width, height) text =
    size width height (Input.button commands.handle command text)

rowHeight = 100

renderSound : Int -> Path -> Instrument -> Sound -> Element
renderSound w path inst sound = case sound of
    Node on -> layers [ button (if on then red else blue) (Flip inst (reverse path)) w rowHeight
                      , if (length path < 4) then
                          container w rowHeight middle (textButton (Split inst (reverse path)) (25, 15) "<>")
                        else empty
                      ]
    Pair l r -> let w' = w `div` 2 in
      layers [ flow right [ renderSound w' (Left::path) inst l
                          , renderSound w' (Right::path) inst r
                          ]
             , if (isNode l && isNode r) then
                 container w rowHeight middle (textButton (Join inst (reverse path)) (25, 15) "><")
               else empty
             ]

renderInst : Instrument -> Sound -> Element
renderInst inst sound =
    flow right [ container 100 rowHeight middle (plainText inst)
               , renderSound 800 [] inst sound
               ]

flipAt : Path -> Sound -> Sound
flipAt path sound = case (path, sound) of
    ([], Node b)          -> Node (not b)
    (Left::ds, Pair l r)  -> Pair (flipAt ds l) r
    (Right::ds, Pair l r) -> Pair l (flipAt ds r)
    _                     -> Native.Error.raise "Cannot flip a Pair!"

splitAt : Path -> Sound -> Sound
splitAt path sound = case (path, sound) of
    ([], Node b)          -> Pair (Node b) (Node b)
    (Left::ds, Pair l r)  -> Pair (splitAt ds l) r
    (Right::ds, Pair l r) -> Pair l (splitAt ds r)
    _                     -> Native.Error.raise "Cannot split a Pair!"

joinAt : Path -> Sound -> Sound
joinAt path sound = case (path, sound) of
    ([], Pair (Node a) (Node b)) -> Node (a || b)
    (Left::ds, Pair l r)         -> Pair (joinAt ds l) r
    (Right::ds, Pair l r)        -> Pair l (joinAt ds r)
    _                            -> Native.Error.raise "Cannot join a Node!"

maybeBind : (a -> b) -> Maybe a -> Maybe b
maybeBind f fm = case fm of
    Nothing -> Nothing
    Just x -> Just (f x)

step : Command -> State -> State
step command state = case command of
    Noop -> state
    Flip inst path  -> Dict.update inst (maybeBind (flipAt path)) state
    Split inst path -> Dict.update inst (maybeBind (splitAt path)) state
    Join inst path  -> Dict.update inst (maybeBind (joinAt path)) state

app : State -> Element
app state = flow down (mapD renderInst state)

initial = Dict.fromList [ ("kick", Node False)
                        , ("hi-hat", Node False)
                        , ("snare", Node False)
                        , ("ping", Node False)
                        , ("shaker", Node False)
                        , ("clap", Node False)
                        ]

currentState = foldp step initial commands.signal

main : Signal Element
main = app <~ currentState

port tellServer : Signal Json.Value
port tellServer = serialize <~ currentState
