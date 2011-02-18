{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
module Data.JSON.Validate
    ( Validation(..)
    , ValidationResult(..)
    , RegexValidation(..)
    , Object
    , Array
    , (:&:)(..)
    , (:|:)(..)
    , string
    , number
    , boolean
    , nullable
    , date
    , dateTime
    , timezone
    , optional
    , required
    , minlength
    , maxlength
    , objectWith
    , arrayOf
    )
where

import Data.JSON.Types ( Root(..), Value(..), Atom(..) )
import qualified Data.JSON.Types as J
import qualified Data.Text.Lazy as T
import Data.Text.Lazy (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (intercalate)
import Data.Int (Int64)


import Text.Regex


-- Result of a validation
data ValidationResult a = Valid
              | Invalid String
                deriving Show


class Validation a where
    validate :: a -> Root -> ValidationResult ()
    validate v (RootObject o) = validateObject v o
    validate v (RootArray  a) = validateArray v a

    validateValue :: a -> Value -> ValidationResult ()
    validateValue v (ValueObject o) = validateObject v o
    validateValue v (ValueArray  a) = validateArray  v a
    validateValue v (ValueAtom   a) = validateAtom   v a

    validateAtom :: a -> Atom -> ValidationResult ()
    validateAtom v (AtomNull     ) = validateNull    v
    validateAtom v (AtomBoolean b) = validateBoolean v b
    validateAtom v (AtomNumber  n) = validateNumber  v n
    validateAtom v (AtomText    s) = validateString  v s

    validateObject  :: a -> J.Object -> ValidationResult ()
    validateObject  _ _ = Invalid "unexpected object"

    validateArray   :: a -> J.Array -> ValidationResult ()
    validateArray  _ _ = Invalid "unexpected array"

    validateString  :: a -> Text -> ValidationResult ()
    validateString  _ _ = Invalid "unexpected string"

    validateNumber  :: a -> Rational -> ValidationResult ()
    validateNumber  _ _ = Invalid "unexpected number"

    validateBoolean :: a -> Bool -> ValidationResult ()
    validateBoolean  _ _ = Invalid "unexpected boolean"

    validateNull    :: a -> ValidationResult ()
    validateNull  _ = Invalid "unexpected null"




-- | Wrapper type used by Object and Array
data AnyValidation = forall x. Validation x => AnyValidation x

instance Validation AnyValidation where
    validate (AnyValidation x) = validate x
    validateValue (AnyValidation x) = validateValue x
    validateAtom (AnyValidation x) = validateAtom x


-- | Represents a JSON Object Schema
data Object = Object (Map Text (Bool, AnyValidation))

-- | Constructor for JSON Object Schemas
objectWith :: [(Text, (Bool, AnyValidation))] -> Object
objectWith = Object . Map.fromList

instance Validation Object where
    validateObject (Object schema) object =
        case loop (Map.toAscList object) (Map.toAscList schema) of
          []    -> Valid
          pairs -> let lines = [concat ["\t", show a,":\t", b] | (a,b) <- pairs]
                       msg = concat ["{\n", intercalate ",\n" lines, "\n}"]
                   in Invalid msg
        where
          loop [] [] = []

          loop [] ((skey,(required,_)):ss) =
              if required
                 then (skey, "missing required key"):loop [] ss
                 else loop [] ss

          loop ((okey,_):os) [] = (okey, "unexpected key"):loop os []

          loop ol@((okey,o):os) sl@((skey,(required, s)):ss) =
              case compare okey skey of
                LT -> (okey, "unexpected key"):loop os sl

                EQ -> case validateValue s o of
                        Valid     -> loop os ss
                        Invalid e -> (okey, e):loop os ss

                GT -> if required
                        then (skey, "missing required key"):loop ol ss
                        else loop ol ss



-- | Represents a JSON Array Schema
data Array = Array AnyValidation

-- | Constructor for JSON Array Schemas
arrayOf :: Validation a => a -> Array
arrayOf = Array . AnyValidation


instance Validation Array where
    validateObject _ _  = Invalid "Expected array, found object"
    validateString _ _  = Invalid "Expected array, found string"
    validateNumber _ _  = Invalid "Expected array, found number"
    validateBoolean _ _ = Invalid "Expected array, found boolean"
    validateNull _      = Invalid "Expected array, found null"

    validateArray (Array schema) array =
        case loop (0::Int) array of
          Valid -> Valid
          Invalid e -> Invalid $ concat ["[\n", e, "\n]"]
        where
          loop _ []     = Valid
          loop i (x:xs) =
              case validateValue schema x of
                Valid     -> loop (i+1) xs
                Invalid e ->
                    let local = concat ["\t#", show i, "\t", e]
                    in case loop (i+1) xs of
                         Valid      -> Invalid local
                         Invalid e' -> Invalid $ concat [local, ",\n", e']







-- | A validation that a JSON value is a string
string :: StringValidation
string = StringValidation

data StringValidation = StringValidation

instance Validation StringValidation where
    validateString _ _ = Valid

-- | A validation that a JSON value is a number
number :: NumberValidation
number = NumberValidation

data NumberValidation = NumberValidation

instance Validation NumberValidation where
    validateNumber _ _ = Valid


-- | A validation that a JSON value is a boolean
boolean :: Booleanvalidation
boolean = Booleanvalidation

data Booleanvalidation = Booleanvalidation

instance Validation Booleanvalidation where
    validateBoolean _ _ = Valid

-- | A utility function for expressing that a JSON value may be null
nullable :: (Validation a) => a -> ((:|:) a IsJsonNull)
nullable x = x :|: IsJsonNull

data IsJsonNull = IsJsonNull

instance Validation IsJsonNull where
    validateNull _ = Valid


-- Length restrictions
data MinLength = MinLength Int64
data MaxLength = MaxLength Int64

minlength :: Int64 -> MinLength
minlength = MinLength

maxlength :: Int64 -> MaxLength
maxlength = MaxLength


instance Validation MinLength where
    validateString (MinLength x) s =
        if T.length s >= x
           then Valid
           else Invalid $ "min length " ++ show x

instance Validation MaxLength where
    validateString (MaxLength x) s =
        if T.length s <= x
           then Valid
           else Invalid $ "max length " ++ show x


data RegexValidation = RegexValidation Regex String

instance Validation RegexValidation where
    validateString (RegexValidation re err) s =
        case matchRegex re $ T.unpack s of
          Just _  -> Valid
          Nothing -> Invalid $ concat [err, ": ", show s]


dateTime :: RegexValidation
dateTime = RegexValidation (mkRegex "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}([.][0-9]+)?(Z|[-+][0-9]{2}:[0-9]{2})?$") "Invalid dateTime"


date :: RegexValidation
date = RegexValidation (mkRegex "^[0-9]{4}-[0-9]{2}-[0-9]{2}$") "Invalid date"


timezone :: RegexValidation
timezone = RegexValidation (mkRegex "^(Z|[-+][0-9]{2}:[0-9]{2})$") "Invalid timezone"



-- | A function for expressing that the presence of a key in an object
-- is optional.
optional :: Validation a => a -> (Bool, AnyValidation)
optional x = (False, AnyValidation x)

-- | A function for expressing that the presence of a key in an object
-- is required.
required :: Validation a => a -> (Bool, AnyValidation)
required x = (True, AnyValidation x)


-- | A combinator for 'and'ing validations
data (:&:) a b = a :&: b

instance (Validation a, Validation b) => Validation ((:&:) a b) where
    validate (a :&: b) root =
        case validate a root of
          Valid -> validate b root
          Invalid ia -> case validate b root of
                          Valid      -> Invalid ia
                          Invalid ib -> Invalid $ concat ["and(",ia,", ",ib,")"]

    validateValue (a :&: b) value =
        case validateValue a value of
          Valid -> validateValue b value
          Invalid ia -> case validateValue b value of
                          Valid      -> Invalid ia
                          Invalid ib -> Invalid $ concat ["and(",ia,", ",ib,")"]

    validateAtom (a :&: b) atom =
        case validateAtom a atom of
          Valid -> validateAtom b atom
          Invalid ia -> case validateAtom b atom of
                          Valid      -> Invalid ia
                          Invalid ib -> Invalid $ concat ["and(",ia,", ",ib,")"]


-- | A combinator for 'or'ing validations
data (:|:) a b = a :|: b

instance (Validation a, Validation b) => Validation ((:|:) a b) where
    validate (a :|: b) root =
        case validate a root of
          Valid      -> Valid
          Invalid ia -> case validate b root of
                          Valid -> Valid
                          Invalid ib -> Invalid $ concat ["or(",ia,", ",ib,")"]

    validateValue (a :|: b) value =
        case validateValue a value of
          Valid      -> Valid
          Invalid ia -> case validateValue b value of
                          Valid -> Valid
                          Invalid ib -> Invalid $ concat ["or(",ia,", ",ib,")"]

    validateAtom (a :|: b) atom =
        case validateAtom a atom of
          Valid      -> Valid
          Invalid ia -> case validateAtom b atom of
                          Valid -> Valid
                          Invalid ib -> Invalid $ concat ["or(",ia,", ",ib,")"]


