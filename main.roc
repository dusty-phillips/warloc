app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.12.0/Lb8EgiejTUzbggO2HVVuPJFkwvvsfW6LojkLR20kTVE.tar.br",
}

import pf.Task
import pf.File
import pf.Arg
import pf.Path
import pf.Stderr

import Tokenizer
import Parser

compile : Str -> Result (List U8) [TokenizeError (List Tokenizer.Error), ParseError (List Parser.Error)]
compile = \input ->
    input
    |> Tokenizer.tokenize
    |> Result.mapErr TokenizeError
    |> Result.try \tokens ->
        tokens
        |> Parser.parse
    |> Result.map \expression ->
        dbg expression

        Str.toUtf8 "TODO: Compile Input"

writeWithWasmExtension : List U8, Str -> Task.Task {} [Exit I32 Str]
writeWithWasmExtension = \bytes, inputFilename ->

    outputPath =
        inputFilename
        |> Path.fromStr
        |> Path.withExtension "wasm"

    outputPath
    |> Path.writeBytes bytes
    |> Task.mapErr \_ -> Exit 5 "Unable to write $(outputPath |> Path.display)"

formatErrors : List { message : Str, position : { row : U32, column : U32 } } -> Str
formatErrors = \errors ->
    Str.joinWith
        (
            errors
            |> List.map \error ->
                row = error.position.row |> Num.toStr
                column = error.position.column |> Num.toStr
                "$(row):$(column) $(error.message)"
        )
        "\n"

main : Task.Task {} [Exit I32 Str]
main =
    when Arg.list! {} is
        [] | [_] -> Task.err (Exit 1 "No input filename provided")
        [_, _, _, ..] -> Task.err (Exit 2 "Too many arguments")
        [_, filename] ->
            compileResult =
                (
                    filename
                    |> File.readUtf8
                    |> Task.mapErr \error ->
                        when error is
                            FileReadErr _ NotFound ->
                                Exit 3 "$(filename) does not exist"

                            FileReadErr _ _ ->
                                Exit 99 "Error reading $(filename)"

                            FileReadUtf8Err _ _ ->
                                Exit 4 "Unable to read UTF8 in $(filename)"
                )!
                    |> compile

            when compileResult is
                Ok compiledBytes -> writeWithWasmExtension compiledBytes filename
                Err (TokenizeError errors) | Err (ParseError errors) ->
                    errors
                    |> formatErrors
                    |> Stderr.line
                    |> Task.mapErr \_ -> Exit 99 "System is failing"
