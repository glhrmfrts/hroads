module Level (
             ) where

data TouchEffect
  = TouchEffectNone
  | TouchEffectAccelerate
  | TouchEffectDeaccelerate
  | TouchEffectKill
  | TouchEffectSlide
  | TouchEffectRefuelOxygen
  deriving (Eq, Ord, Show)
