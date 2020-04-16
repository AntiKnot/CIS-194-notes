data Thing = Shoe
 | Ship
 | SealingWax
 | Cabbage
 | King
 deriving Show

data FailableDouble = Failure
                    | Ok Double
 deriving Show

safeDiv :: Double-> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = Ok (x/y)

failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (Ok d) = d
 
       
