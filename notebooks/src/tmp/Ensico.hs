module Ensico where

import Data.List

desenha = putStrLn . ps where
    ps = intercalate "\n" . map p
    p = (>>= f)
    w = "  "
    b = "\9632\9632"
    f 0 = w
    f 1 = b

