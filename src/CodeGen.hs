{-|
Module      : CodeGen
Description : The code generation module
Copyright   : (c) Jonathan Tanner, 2019
Licence     : GPL-3
Maintainer  : jonathan.tanner@sjc.ox.ac.uk
Stability   : experimental
-}
module CodeGen
  ( genCode
  ) where

import Proc (Proc)
import Verilog.GenAST (genAST)
import Verilog.Optimiser (optimise)
import Verilog.Output (output)

-- | Generate the verilog code for the given processor
genCode :: Proc -> String
genCode = output 0 . optimise . genAST

