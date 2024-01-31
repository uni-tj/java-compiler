module SemanticCheck.StdLib where

import Types.AST
import qualified Types.Core as Core

printStream :: Class
printStream =
  Class
    { cname = "java/io/PrintStream",
      cmethods =
        [ printFunc "print" Char,
          printFunc "print" Int,
          printFunc "print" $ Instance "java/lang/Object",
          printFunc "println" Bool,
          printFunc "println" Char,
          printFunc "println" Int,
          printFunc "println" $ Instance "java/lang/Object"
        ],
      cfields = [],
      cextends = Nothing,
      cconstructors =
        [ Constructor {crparams = [], crname = "System", crbody = Block [], craccess = Public}
        ],
      caccess = Public
    }

printFunc :: String -> Type -> Method
printFunc mname aType =
  Method
    { mtype = Core.Void,
      mstatic = True,
      mparams = [(aType, "x")],
      moverride = False,
      mname = mname,
      mbody = Block [],
      maccess = Public
    }

systemClass :: Class
systemClass =
  Class
    { cname = "java/lang/System",
      cmethods = [],
      cfields =
        [ Field {ftype = Core.Class "java/io/PrintStream", fstatic = True, foverride = False, fname = "out", finit = Nothing, faccess = Public}
        ],
      cextends = Just "java/lang/Object",
      cconstructors =
        [ Constructor {crparams = [], crname = "java/io/PrintStream", crbody = Block [], craccess = Public}
        ],
      caccess = Public
    }
