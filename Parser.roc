module [
    parse,
    Error,
]

Position : {
    row : U32,
    column : U32,
}
Error : {
    message : Str,
    position : Position,
}

parse : List _ -> Result _ [ParseError (List Error)]
parse = \_tokens ->
    Err (ParseError [{ message: "I don't know", position: { row: 1, column: 1 } }])
