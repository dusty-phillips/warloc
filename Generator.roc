module [
    generate,
]

import Transformer
import Common

generate : Common.Positionable Transformer.Module -> Result (List U8) (List Common.Error)
generate = \module ->
    Ok (Str.toUtf8 "TODO: Compile Input")
