module CustomFields.Categoria where 

import Database.Persist.TH
import Data.Eq
import Text.Show
import Text.Read

data Categoria = Programacao | Seguranca | Tecnologia
    deriving (Show, Read, Eq)
derivePersistField "Categoria"