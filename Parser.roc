module [
    parse,
    Expression,
    Node,
]

import Common
import Tokenizer

Term : [Number U32, String Str, Name Str, Variable Str]
Expression : [
    Term Term,
    SExpression
        {
            name : Str,
            children : List Expression,
        },
]

Node : {
    expression : Expression,
    position : Common.Position,
}

ParseState : {
    openNodeStack : List Node,
    errors : List Common.Error,
}

initialParseState : ParseState
initialParseState = {
    openNodeStack: [],
    errors: [],
}

parse : List Tokenizer.Token -> Result Node (List Common.Error)
parse = \tokens ->
    parseResult = parseRecursive tokens initialParseState
    # dbg parseResult

    when parseResult is
        { errors: [], openNodeStack: [{ expression: SExpression sExpression, position }] } ->
            Ok { expression: SExpression sExpression, position }

        { errors: [], openNodeStack: [{ expression: Term _, position }] } ->
            Err [{ position, message: "Expected initial module expression, received a term" }]

        { errors: [], openNodeStack: [] } ->
            Err [{ position: { row: 1, column: 1 }, message: "Missing module expression" }]

        { errors: [], openNodeStack: [..] } ->
            Err [{ position: { row: 1, column: 1 }, message: "Too many expressions" }]

        { errors, openNodeStack: [..] } -> Err errors

parseRecursive : List Tokenizer.Token, ParseState -> ParseState
parseRecursive = \tokens, currentState ->
    # dbg tokens

    (nextTokens, nextState) =
        when (tokens, currentState.openNodeStack) is
            ([{ position, token: LParen }, { token: Name name }, .. as rest], currentNodeStack) ->
                (
                    rest,
                    { currentState &
                        openNodeStack: List.append currentNodeStack {
                            position,
                            expression: SExpression ({ name, children: [] }),
                        },
                    },
                )

            ([{ token: RParen }, .. as rest], [_singleRootNode]) ->
                (rest, currentState)

            ([{ position, token: RParen }, .. as rest], []) ->
                message = "Unmatched ')'"
                (
                    rest,
                    { currentState &
                        errors: List.append currentState.errors { position, message },
                    },
                )

            ([], _) -> crash "Empty tokens should never recurse"
            ([token, .. as rest], _) ->
                message = "Unexpected Token $(token |> Tokenizer.toStr)"
                (
                    rest,
                    { currentState &
                        errors: List.append currentState.errors { position: token.position, message },
                    },
                )

    when nextTokens is
        [] -> nextState
        _ -> parseRecursive nextTokens nextState

expect
    # (module)
    tokens = [
        { position: { row: 1, column: 1 }, token: LParen },
        { position: { row: 1, column: 2 }, token: Name "module" },
        { position: { row: 1, column: 8 }, token: RParen },
    ]

    result : Result Node (List Common.Error)
    result = parse tokens

    result
    == Ok {
        position: { row: 1, column: 1 },
        expression: SExpression { name: "module", children: [] },
    }

expect
    # )
    tokens = [
        { position: { row: 1, column: 1 }, token: RParen },
    ]

    result : Result Node (List Common.Error)
    result = parse tokens

    result
    == Err [
        {
            position: { row: 1, column: 1 },
            message: "Unmatched ')'",
        },
    ]

expect
    # (module))
    tokens = [
        { position: { row: 1, column: 1 }, token: LParen },
        { position: { row: 1, column: 2 }, token: Name "module" },
        { position: { row: 1, column: 8 }, token: RParen },
        { position: { row: 1, column: 9 }, token: RParen },
    ]

    result : Result Node (List Common.Error)
    result = parse tokens

    result
    == Err [
        {
            position: { row: 1, column: 9 },
            message: "Unmatched ')'",
        },
    ]

expect
    # (module
    tokens = [
        { position: { row: 1, column: 1 }, token: LParen },
        { position: { row: 1, column: 2 }, token: Name "module" },
    ]

    result : Result Node (List Common.Error)
    result = parse tokens

    result
    == Err [
        {
            position: { row: 1, column: 9 },
            message: "Incomplete expression, no closing ')'",
        },
    ]
