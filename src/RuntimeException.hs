module RuntimeException (
  RuntimeException(..),
  ErrorMsg
) where

{- "Data" library import(s). -}
import Data.Text (Text(..))

{- "Control" library import(s). -}
import Control.Exception.Base (Exception)

type ErrorMsg = Text

data RuntimeException
  = InsufficientArgs          ErrorMsg
  | TooManyArgs               ErrorMsg
  | InvalidArg                ErrorMsg
  | StatisticGatheringFailure ErrorMsg
  | UnknownException          ErrorMsg
  deriving (Show)

instance Exception RuntimeException