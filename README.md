# bower-json

A package that provides a data type and functions for working with bower.json
package manifest files.

Example usage:

```haskell
import System.IO
import System.Exit

import Web.BowerJson

main = do
  eBowerJson <- decodeFile "bower.json"
  case eBowerJson of
    Right bowerJson ->
      if bowerPrivate bowerJson
        then hPutStrLn stderr "error: package is private" >> exitFailure
        else putStrLn (runPackageName (bowerName bowerJson))
    Left err -> do
      hPutStrLn stderr ("error: failed to parse bower.json: " ++ err)
      exitFailure
```

See the [documentation on Hackage](https://hackage.haskell.org/package/bower-json) for more details.
