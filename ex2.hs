import Prelude hiding ((&&))

a && b = if a then if b then False else True else False