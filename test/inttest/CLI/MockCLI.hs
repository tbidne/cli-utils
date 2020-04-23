module CLI.MockCLI
  ( MonadCLI (..),
  )
where

import CLI.MonadCLI
import Output

instance MonadCLI Output where
  runCommands xs = Output xs ()
