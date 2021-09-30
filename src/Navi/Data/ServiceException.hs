module Navi.Data.ServiceException
  ( ServiceEx (..),
    toServiceEx,
    fromServiceEx,
  )
where

import Control.Exception (Exception (..), SomeException)
import Data.Typeable (cast)
import Navi.Class.Service (Service (..))

data ServiceEx where
  MkServiceEx :: forall e. (Exception e, Service e) => e -> ServiceEx

instance Show ServiceEx where
  show (MkServiceEx e) = "MkServiceEx " <> show e

instance Exception ServiceEx where
  displayException (MkServiceEx (err :: e)) =
    "Encountered service error in service <"
      <> sname
      <> ">: "
      <> displayException err
    where
      sname = name @e

toServiceEx :: (Exception e, Service e) => e -> SomeException
toServiceEx = toException . MkServiceEx

fromServiceEx :: Exception e => SomeException -> Maybe e
fromServiceEx x = do
  MkServiceEx e <- fromException x
  cast e