data RPS = Rock | Paper | Scissors deriving (Show)

beats :: RPS -> RPS -> Bool
beats Paper Rock = True
beats Rock Scissors = True
beats Scissors Paper = True
beats _ _ = False