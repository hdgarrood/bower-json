# bower-json

A package that provides a data type and functions for working with bower.json
package manifest files.

Example usage:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import System.IO
import System.Exit
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Web.Bower.PackageMeta

main = do
  ePkgMeta <- decodeFile "bower.json"
  case ePkgMeta of
    Right pkgMeta ->
      if bowerPrivate pkgMeta
        then T.hPutStrLn stderr "error: package is private" >> exitFailure
        else putStrLn (runPackageName (bowerName pkgMeta))
    Left err -> do
      T.hPutStrLn stderr ("error: failed to parse bower.json: " `T.append` displayError err)
      exitFailure
```

See the [documentation on Hackage](https://hackage.haskell.org/package/bower-json) for more details.
