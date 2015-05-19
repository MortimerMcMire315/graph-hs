module GraphTheory.Misc.Infinity where

data Infinitable a = Regular a | NegativeInfinity | PositiveInfinity

type InfInt = Infinitable Integer
type Inf a = Infinitable a

instance Ord a => Ord (Infinitable a) where
    compare NegativeInfinity NegativeInfinity = EQ
    compare PositiveInfinity PositiveInfinity = EQ
    compare NegativeInfinity _ = LT
    compare PositiveInfinity _ = GT
    compare _ PositiveInfinity = LT
    compare _ NegativeInfinity = GT
    compare (Regular x) (Regular y) = compare x y

instance Num a => Num (Infinitable a) where
    (Regular a) + (Regular b) = Regular (a + b)
    PositiveInfinity + PositiveInfinity = PositiveInfinity
    PositiveInfinity + (Regular a) = PositiveInfinity
    (Regular a) + PositiveInfinity = PositiveInfinity
    (Regular a) + NegativeInfinity = NegativeInfinity
    NegativeInfinity + NegativeInfinity = NegativeInfinity
    NegativeInfinity + (Regular a) = NegativeInfinity
    _ + _ = error "Attempted to add ∞ + -∞."

    (Regular a) - (Regular b) = Regular (a - b)
    _ - _ = error "Attempted to subtract two infinite values."
    
    (Regular a) * (Regular b) = Regular (a * b)
    _ * _ = error "Attempted to multiply two infinite values."

    negate (Regular a) = Regular $ negate a
    negate PositiveInfinity = NegativeInfinity
    negate NegativeInfinity = PositiveInfinity

    fromInteger a = Regular (fromInteger a)

    signum (Regular a) = Regular (signum a)
    
    abs (Regular a) = Regular (abs a)
    abs PositiveInfinity = PositiveInfinity
    abs NegativeInfinity = PositiveInfinity

instance Show a => Show (Infinitable a) where
    show (Regular a) = show a
    show PositiveInfinity = "∞"
    show NegativeInfinity = "-∞"

instance Eq a => Eq (Infinitable a) where
    PositiveInfinity == PositiveInfinity = True
    NegativeInfinity == NegativeInfinity = True
    Regular a == Regular b = (a == b)
    _ == _ = False
