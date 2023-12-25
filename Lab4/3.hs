data Nat = Zero | Suc Nat deriving (Show)

add :: Nat -> Nat -> Nat
add x Zero = x
add Zero y = y
add x (Suc base) = add (Suc x) base

mult :: Nat -> Nat -> Nat
mult x Zero = Zero
mult Zero y = Zero
mult (Suc x) y = add (mult x y) y

equal :: Nat -> Nat -> Bool
equal Zero x = False
equal x Zero = False
equal Zero Zero = True
equal x y = equal (Suc x) (Suc y)