module CustomTypeAndTypeClass
where 

import qualified Data.Map as Map


customTypeAndTypeClassFunc = putStrLn "CustomTypeAndTypeClass"

data Point = Point Float Float deriving (Show)  
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)  
data Triagle = Triagle Float Float Float deriving (Show)

surface :: Shape -> Float
surface ( Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)  

nudge :: Shape -> Float -> Float -> Shape  
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r  
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

baseCircle :: Float -> Shape  
baseCircle r = Circle (Point 0 0) r  
  
baseRect :: Float -> Float -> Shape  
baseRect width height = Rectangle (Point 0 0) (Point width height)  


--Record Syntax
data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     , height :: Float  
                     , phoneNumber :: String  
                     , flavor :: String  
                     } deriving (Show, Eq)   

data Car = Car {company :: String, model :: String, year :: Maybe Int} deriving (Show)

-- Type constructor and Values Constructor
-- In a data declaration, a type constructor is the thing on the left hand side of the equals sign. 
-- The data/values constructor(s) are the things on the right hand side of the equals sign. 
-- You use type constructors where a type is expected, and you use data constructors where a value is expected

data Vector a = Vector a a a deriving (Show)
vplus :: (Num t) => Vector t -> Vector t -> Vector t  
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)  
  
vectMult :: (Num t) => Vector t -> t -> Vector t  
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)  
  
scalarMult :: (Num t) => Vector t -> Vector t -> t  
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday   
           deriving (Eq, Ord, Show, Read, Bounded, Enum)  

-- Type Synopym
type PhoneNumber = String  
type Name = String  
type PhoneBook = [(Name,PhoneNumber)]  

phoneBook :: PhoneBook
phoneBook = [("betty","555-2938")     
            ,("bonnie","452-2928")     
            ,("patsy","493-2928")     
            ,("lucille","205-2928")     
            ,("wendy","939-8282")     
            ,("penny","853-2492")     
            ]  

inPhoneBook :: Name -> PhoneNumber -> Bool
inPhoneBook name number = (name,number) `elem` phoneBook

type AssocList k v = [(k,v)]

type IntMap v = Map.Map Int v

--data Either' a b = Left a | Right b deriving (Eq, Ord, Read, Show)
data Either1 a = Nothing' | Just' a

--Locker example
--type Locker = LockerState Code

--data LockerState = Taken | Free deriving (Show, Eq)

-- type Code = String 
-- type LockerMap = Map.Map Int Locker 

-- data Locker =  Locker { 
--                         state :: LockerState
--                       , code :: String
--                       } deriving (Show, Eq)

-- data LockersMap = LockersMap { 
--                             map :: LockerMap
--                           , locker :: Locker
--                         } deriving (Show, Eq)
data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber maps = 
  let locker = Map.lookup lockerNumber maps
  in 
    case locker of 
      Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
      Just (state, code) -> if state /= Taken
                            then Right code 
                            else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

--let lockers = Map.fromList[(100,(Taken,"ZD39I")),(101,(Free,"JAH3I")),(103,(Free,"IQSA9")),(105,(Free,"QOTSA")),(109,(Taken,"893JJ")),(110,(Taken,"99292"))]  
--- Recursive Data Structure ---
data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
--data List' a = Empty' | Cons' {listHead :: a, listTail :: List' a} deriving (Show, Read, Eq, Ord)

-- Binary Search tree
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right) 
              | x == a = Node x left right
              | x < a = Node a (treeInsert x left) right
              | x > a = Node a left (treeInsert x right)







---- JSON 
data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
              deriving (Eq, Ord, Show)

getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _           = Nothing

getInt (JNumber n) = Just (truncate n)
getInt _           = Nothing

getDouble (JNumber n) = Just n
getDouble _           = Nothing

getBool (JBool b) = Just b
getBool _         = Nothing

getObject (JObject o) = Just o
getObject _           = Nothing

getArray (JArray a) = Just a
getArray _          = Nothing

isNull v            = v == JNull