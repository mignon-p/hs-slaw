module Data.Slaw.Internal.Validation
  ( ValidationFlag(..)
  , ValidationFlags
  , validateSlaw
  ) where

import Control.DeepSeq
-- import Control.Exception
-- import qualified Data.ByteString.Lazy as L
-- import Data.Default.Class
import Data.Hashable
import GHC.Generics (Generic)

import Data.Slaw.Internal.Exception
import Data.Slaw.Internal.SlawType
import Data.Slaw.Internal.String

data ValidationFlag = VfCSlaw | VfUtf8 | VfDesIng
                    deriving (Eq, Ord, Show, Read, Bounded, Enum,
                              Generic, NFData, Hashable)

type ValidationFlags = [ValidationFlag]
type ValRet          = Either PlasmaException ()

vs :: ValidationFlags -> Slaw -> ValRet
vs f     (SlawProtein d i r  ) = vsProtein f d i r
vs _     (SlawBool    _      ) = return ()
vs _      SlawNil              = return ()
vs f     (SlawList    ss     ) = mapM_ (vs     f) ss
vs f     (SlawMap     pairs  ) = mapM_ (vsPair f) pairs
vs f     (SlawCons    car cdr) = vsPair f (car, cdr)
vs f     (SlawNumeric nf  nd ) = vsNumeric f nf nd
vs _     (SlawError   msg loc) = vsError   msg loc
vs f     (SlawSymbol  sym    )
  | VfCSlaw `elem` f           = cslawErr "symbols"
  | otherwise                  = vsSymbol sym
vs f     (SlawString  utf8   )
  | VfUtf8 `elem` f            = vsString utf8
  | otherwise                  = return ()

vsProtein :: ValidationFlags
          -> Maybe Slaw
          -> Maybe Slaw
          -> RudeData
          -> ValRet
vsProtein = undefined

vsSymbol :: Symbol -> ValRet
vsSymbol = undefined

vsString :: Utf8Str -> ValRet
vsString = undefined

vsPair :: ValidationFlags -> (Slaw, Slaw) -> ValRet
vsPair = undefined

vsNumeric :: ValidationFlags -> NumericFormat -> NumericData -> ValRet
vsNumeric = undefined

vsError :: String -> ErrLocation -> ValRet
vsError = undefined

cslawErr :: String -> ValRet
cslawErr = undefined

validateSlaw :: ValidationFlags -> Slaw -> Either PlasmaException ()
validateSlaw = vs
