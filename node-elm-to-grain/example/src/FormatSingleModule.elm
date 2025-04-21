module FormatSingleModule exposing (formatSingleModule)

import ElmSyntaxParserLenient
import ElmSyntaxPrint


formatSingleModule : String -> Maybe String
formatSingleModule originalSource =
    case
        originalSource
            |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.module_
    of
        Just syntaxModule ->
            syntaxModule
                |> ElmSyntaxPrint.module_
                |> ElmSyntaxPrint.toString
                |> Just
        
        Nothing ->
            Nothing
