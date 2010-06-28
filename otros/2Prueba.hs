module Main (main) where 

data A = Hola Int
       | Lista A
       deriving (Show)

data C = Cd Int
       | Nothingness
       deriving (Show)

data B = Maybe
       | ListaB B Maybe
       deriving (Show)

main = return ()
