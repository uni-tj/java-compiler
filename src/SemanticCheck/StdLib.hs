module SemanticCheck.StdLib (stdLib) where

import           Types.AST
import qualified Types.Core as Type
import           Types.Core (AccessModifier (..), Type)

stdLib :: [Class]
stdLib = [object, system, printStream]

dummyPosition :: Position
dummyPosition  = Position{ start = (0,0), end = (0,0) }

object :: Class
object = Class
  { caccess       = Public
  , cname         = "java/lang/Object"
  , cextends      = Nothing
  , cfields       = []
  , cmethods      = []
  , cconstructors = [ Constructor Public "java/lang/Object" [] $ Block dummyPosition [] ]
  }

printStream :: Class
printStream =
  Class
    { cname = "java/io/PrintStream",
      cmethods =
        [ printFunc "print" Type.Char,
          printFunc "print" Type.Int,
          printFunc "print" $ Type.Instance "java/lang/Object",
          printFunc "println" Type.Bool,
          printFunc "println" Type.Char,
          printFunc "println" Type.Int,
          printFunc "println" $ Type.Instance "java/lang/Object"
        ],
      cfields = [],
      cextends = Nothing,
      cconstructors =
        [ Constructor {crparams = [], crname = "System", crbody = Block dummyPosition [], craccess = Public}
        ],
      caccess = Public
    }

printFunc :: String -> Type -> Method
printFunc mname ptype =
  Method
    { mtype  = Type.Void,
      mstatic = False,
      mparams = [(ptype, "x")],
      moverride = False,
      mname = mname,
      mbody = Block dummyPosition [],
      maccess = Public
    }

system :: Class
system =
  Class
    { cname = "java/lang/System",
      cmethods = [],
      cfields =
        [ Field {ftype = Type.Instance "java/io/PrintStream", fstatic = True, foverride = False, fname = "out", finit = Nothing, faccess = Public}
        ],
      cextends = Just "java/lang/Object",
      cconstructors =
        [ Constructor {crparams = [], crname = "java/io/PrintStream", crbody = Block dummyPosition [], craccess = Public}
        ],
      caccess = Public
    }
