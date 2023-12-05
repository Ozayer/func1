module Eq3 (Eq3((===))) where
import Bool3

class Eq3 a where
  (===) :: a -> a -> Bool3

instance Eq3 Bool3 where
  (===) True3 True3 = True3
  (===) False3 False3 = True3
  (===) Unk3 _ = Unk3
  (===) _ Unk3 = Unk3
  (===) _ _ = False3

instance Eq3 a => Eq3 (Maybe a) where
  (===) Nothing _ = Unk3
  (===) _ Nothing = Unk3
  (===) (Just a1) (Just a2) = a1 === a2
