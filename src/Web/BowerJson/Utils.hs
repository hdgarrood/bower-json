
module Web.BowerJson.Utils where

import Control.Applicative

--------------
-- Safe

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:xs) = Just x

lastMay :: [a] -> Maybe a
lastMay [] = Nothing
lastMay [x] = Just x
lastMay (x:xs) = lastMay xs

------------------------------------------------
-- Reimplementations of Lens types/functions

type Getting r s a = (a -> Const r a) -> s -> Const r s

(^.) :: s -> Getting a s a -> a
(^.) x l = getConst (l Const x)
