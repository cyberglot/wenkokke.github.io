module Build.Pandoc.Citations where
import Text.Pandoc
import Citeproc.Types
import Build.Prelude

processCitations :: Style Inlines ->
processCitations = _