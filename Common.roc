module [Position, Positionable, Error, formatErrors]

Position : {
    row : U32,
    column : U32,
}

Positionable elem : {
    position : Position,
    element : elem,
}

Error : {
    message : Str,
    position : Position,
}

formatErrors : List Error -> Str
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
