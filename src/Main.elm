import Html exposing (text)

set1 : List (Float, Float)
set1 =
  [ (1, 1), (2, 2), (3, 3) ]


set2 : List (Float, Float)
set2 =
  [ (3, 2), (1, 2), (0, 1), (4, 3) ]


hypothesis theta0 theta1 xi =
  theta0 + theta1 * xi
  

cost: List (Float, Float) -> Float -> Float -> Float
cost set thetha0 thetha1 =
  let
    m: Float
    m =
      set
        |> List.length
        |> toFloat
  
    hypothesis_ xi =
      hypothesis thetha0 thetha1 xi
  in
    (1 / (2 * m)) * ((List.foldr (\(x, y) acc -> (((hypothesis_ x) - y) ^ 2) + acc) 0 set))


newTheta0: List (Float, Float) -> Float -> Float -> Float
newTheta0 set thetha0 thetha1 =
  let
    m: Float
    m =
      set
        |> List.length
        |> toFloat

    hypothesis_ xi =
      hypothesis thetha0 thetha1 xi
  in
    (1 / m) * (List.foldr (\(x, y) acc -> (((hypothesis_ x) - y)) + acc) 0 set)


newTheta1: List (Float, Float) -> Float -> Float -> Float
newTheta1 set thetha0 thetha1 =
  let
    m: Float
    m =
      set
        |> List.length
        |> toFloat

    hypothesis_ xi =
      hypothesis thetha0 thetha1 xi
  in
    (1 / m) * (List.foldr (\(x, y) acc -> (((hypothesis_ x) - y) * x) + acc) 0 set)
  
main =
  [ cost set1 0 0.5
  , cost set2 0 1
  , newTheta0 [ (2000, 100), (3000, 110) ] 10 10
  , newTheta1 set1 0 0
  ]
  |> toString
  |> text

