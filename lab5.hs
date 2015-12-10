import Prelude hiding (and)
and [] = True
and (b:bs) = and bs && b