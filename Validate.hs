module Validate where

data Validate err a = Failure err | Success a deriving (Show, Eq)

instance Functor (Validate err) where
  fmap f (Failure err) = Failure err
  fmap f (Success a  ) = Success (f a)

instance Semigroup err => Applicative (Validate err) where
  pure = Success
  (Failure a) <*> (Failure b) = Failure (a <> b)
  (Failure a) <*> _           = Failure a
  _           <*> (Failure a) = Failure a
  (Success f) <*> (Success a) = Success (f a)

toMaybe :: Validate err a -> Maybe a
toMaybe (Failure _) = Nothing
toMaybe (Success a) = Just a

fromMaybe :: err -> Maybe a -> Validate err a
fromMaybe err Nothing  = Failure err
fromMaybe err (Just a) = Success a

toEither :: Validate err a -> Either err a
toEither (Failure err) = Left err
toEither (Success a  ) = Right a

fromEither :: Either err a -> Validate err a
fromEither (Left  err) = Failure err
fromEither (Right a  ) = Success a

fromPred :: err -> (a -> Bool) -> a -> Validate err a
fromPred err p a
  | p a = Success a
  | otherwise = Failure err