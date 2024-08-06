module Queue where

import Lib (CanBeEmpty (..), FromList(..), ToList(..))

data Queue a = Queue a (Queue a)
             | EmptyQueue
             deriving (Eq)
             

instance Show a => Show (Queue a) where
  -- | Show the queue
  show EmptyQueue = "<-|"
  show (Queue a b) = "<-" ++ show a ++ (show b) -- first show shows a charracter, seccond show shows a list like reccursion

-- instance Show a => Show (Queue a) where
-- -- | Show the queue
-- show EmptyQueue = "EmptyQueue"
-- show (Queue a b) = "Queue " ++ show a ++ "(" ++ show b ++ ")" -- first show shows a charracter, seccond show shows a list like reccursion

instance FromList Queue where
  fromList = foldr Queue EmptyQueue

-- :k Functor, Functor :: (* -> *) -> Constraint
instance Functor Queue where
  fmap _ EmptyQueue = EmptyQueue
  fmap f (Queue x xs) = Queue (f x) (fmap f xs)

instance ToList Queue where
  toList = foldr (:) []

-- :k Foldable, Foldable :: (* -> *) -> Constraint
instance Foldable Queue where
  foldr _ z EmptyQueue = z
  foldr f z (Queue x xs) = f x (foldr f z xs)


-- :k CanBeEmpty, CanBeEmpty :: * -> Constraint
instance CanBeEmpty (Queue a) where
  isEmpty EmptyQueue = True
  isEmpty _ = False


-- :k Semigroup , Semigroup :: (* -> *) -> Constraint
instance Semigroup (Queue a) where
  EmptyQueue <> EmptyQueue = EmptyQueue
  EmptyQueue <> x = x
  x <> EmptyQueue = x
  Queue a as <> Queue x xs = (enqueue x (Queue a as)) <> xs


-- :k Monoid, Monoid :: * -> Constraint
instance Monoid (Queue a) where 
  mempty = EmptyQueue


-- :k Applicative, Applicative :: (* -> *) -> Constraint
instance Applicative Queue where
  pure x = Queue x EmptyQueue
  EmptyQueue <*> _ = EmptyQueue
  _ <*> EmptyQueue = EmptyQueue
  (<*>)  (Queue f fs) (Queue y ys) = (fmap f (Queue y ys)) <> (fs <*> (Queue y ys))

-- (>>=) :: m a -> (a -> m b) -> m b
instance Monad Queue where
  EmptyQueue >>= _ = EmptyQueue
  (Queue a as) >>= f = (f a) <> (as >>= f)




enqueue :: a -> Queue a -> Queue a
enqueue x EmptyQueue = Queue x EmptyQueue
enqueue b (Queue x xs) = Queue x (enqueue b xs)

dequeue :: Queue a -> Queue a
dequeue EmptyQueue = EmptyQueue
dequeue (Queue x xs) = xs

front :: Queue a -> a
front EmptyQueue = error "Empty queue so no front" -- Keine ahnung was ich da machen soll, keine spezifikation in Tests oder Aufgabenstellung gefunden
front (Queue a xs) = a

-- zum Texten erstellte Queues
exampleQueue :: Queue Int
exampleQueue = enqueue 3 (enqueue 2 (enqueue 1 EmptyQueue))
qf = Queue (+1) (Queue (*2) EmptyQueue) :: Queue (Int -> Int)
qx = Queue 1 (Queue 2 (Queue 3 EmptyQueue)) :: Queue Int



