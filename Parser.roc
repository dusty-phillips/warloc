module [
    parse,
    Expression,
]

import Common
import Tokenizer

Term : [Number U32, String Str, Name Str, Variable Str]
Expression : [
    Term
        {
            position : Common.Position,
            term : Term,
        },
    SExpression
        {
            position : Common.Position,
            name : Str,
            children : List Expression,
            state : [Open, Closed],
        },
]

ParseState : {
    openNodeStack : List Expression,
    errors : List Common.Error,
}

initialParseState : ParseState
initialParseState = {
    openNodeStack: [],
    errors: [],
}

parse : List Tokenizer.Token -> Result Expression (List Common.Error)
parse = \tokens ->
    parseResult = parseRecursive tokens initialParseState
    # dbg parseResult

    when parseResult is
        { errors: [], openNodeStack: [SExpression { name, position, children, state: Closed }] } ->
            Ok (SExpression { name, position, children, state: Closed })

        { errors: [], openNodeStack: [SExpression { position, state: Open }] } ->
            Err [
                {
                    position,
                    message: "Incomplete expression, no closing ')'",
                },
            ]

        { errors: [], openNodeStack: [Term { position }] } ->
            Err [
                {
                    position,
                    message: "Expected initial module expression, received a term",
                },
            ]

        { errors: [], openNodeStack: [] } ->
            Err [
                {
                    position: { row: 1, column: 1 },
                    message: "Missing module expression",
                },
            ]

        { errors: [], openNodeStack: [..] } ->
            Err [
                {
                    position: { row: 1, column: 1 },
                    message: "Too many expressions",
                },
            ]

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
                        openNodeStack: List.append
                            currentNodeStack
                            (
                                SExpression {
                                    position,
                                    name,
                                    children: [],
                                    state: Open,
                                }
                            ),
                    },
                )

            ([{ token: LParen }, { token, position }, .. as rest], _) ->
                message = "Expected Name, found '$({ token, position } |> Tokenizer.toStr)'"
                (
                    List.prepend rest { token, position },
                    { currentState &
                        errors: List.append currentState.errors { position, message },
                    },
                )

            # Due to some oddness in roc, this pattern has to go before the term patterns below it.
            ([{ token: RParen }, .. as remainingTokens], [.. as headOfStack, SExpression { position, name, children, state: Open }, SExpression { position: childPosition, name: childName, children: childChildren, state: Open }]) ->
                (
                    remainingTokens,
                    { currentState &
                        openNodeStack: List.append
                            headOfStack
                            (
                                SExpression {
                                    position,
                                    name,
                                    children: List.append
                                        children
                                        (
                                            SExpression {
                                                position: childPosition,
                                                name: childName,
                                                children: childChildren,
                                                state: Closed,
                                            }
                                        ),
                                    state: Open,
                                }
                            ),
                    },
                )

            # Would be nice if these next four cases could be merged into one
            # case that was somehow generic over the token type
            ([{ position: termPosition, token: Number number }, .. as remainingTokens], [.. as headOfStack, SExpression { position, name, children, state: Open }]) ->
                (
                    remainingTokens,
                    { currentState &
                        openNodeStack: List.append
                            headOfStack
                            (
                                SExpression {
                                    position,
                                    name,
                                    children: List.append children (Term { position: termPosition, term: Number number }),
                                    state: Open,
                                }
                            ),
                    },
                )

            ([{ position: termPosition, token: String string }, .. as remainingTokens], [.. as headOfStack, SExpression { position, name, children, state: Open }]) ->
                (
                    remainingTokens,
                    { currentState &
                        openNodeStack: List.append
                            headOfStack
                            (
                                SExpression {
                                    position,
                                    name,
                                    children: List.append children (Term { position: termPosition, term: String string }),
                                    state: Open,
                                }
                            ),
                    },
                )

            ([{ position: termPosition, token: Variable string }, .. as remainingTokens], [.. as headOfStack, SExpression { position, name, children, state: Open }]) ->
                (
                    remainingTokens,
                    { currentState &
                        openNodeStack: List.append
                            headOfStack
                            (
                                SExpression {
                                    position,
                                    name,
                                    children: List.append children (Term { position: termPosition, term: Variable string }),
                                    state: Open,
                                }
                            ),
                    },
                )

            ([{ position: termPosition, token: Name tokenName }, .. as remainingTokens], [.. as headOfStack, SExpression { position, name, children, state: Open }]) ->
                (
                    remainingTokens,
                    { currentState &
                        openNodeStack: List.append
                            headOfStack
                            (
                                SExpression {
                                    position,
                                    name,
                                    children: List.append children (Term { position: termPosition, term: Name tokenName }),
                                    state: Open,
                                }
                            ),
                    },
                )

            # end copy paste distater
            ([{ token: RParen }, .. as rest], [SExpression { position, name, children, state: Open }]) ->
                (
                    rest,
                    { currentState &
                        openNodeStack: [SExpression { position, name, children, state: Closed }],
                    },
                )

            ([{ token: RParen, position }, .. as rest], [.., SExpression { state: Closed }]) ->
                message = "Unmatched ')'"
                (
                    rest,
                    { currentState &
                        errors: List.append currentState.errors { position, message },
                    },
                )

            ([{ position, token: RParen }, .. as rest], []) ->
                message = "Unmatched ')'"
                (
                    rest,
                    { currentState &
                        errors: List.append currentState.errors { position, message },
                    },
                )

            ([], _) -> ([], { openNodeStack: [], errors: [{ position: { row: 0, column: 0 }, message: "No tokens provided" }] })
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

    result = parse tokens

    result
    == Ok
        (
            SExpression {
                name: "module",
                position: { row: 1, column: 1 },
                children: [],
                state: Closed,
            }
        )

expect
    # (module (memory 10))
    tokens = [
        { position: { row: 1, column: 1 }, token: LParen },
        { position: { row: 1, column: 2 }, token: Name "module" },
        { position: { row: 1, column: 9 }, token: LParen },
        { position: { row: 1, column: 10 }, token: Name "memory" },
        { position: { row: 1, column: 17 }, token: Number 10 },
        { position: { row: 1, column: 19 }, token: RParen },
        { position: { row: 1, column: 20 }, token: RParen },
    ]

    result = parse tokens

    result
    == Ok
        (
            SExpression {
                position: { column: 1, row: 1 },
                name: "module",
                state: Closed,
                children: [
                    SExpression {
                        position: { column: 9, row: 1 },
                        name: "memory",
                        state: Closed,
                        children: [Term { position: { column: 17, row: 1 }, term: Number 10 }],
                    },
                ],
            }
        )

expect
    tokens = []

    result = parse tokens

    result
    == Err [
        {
            position: { row: 0, column: 0 },
            message: "No tokens provided",
        },
    ]

expect
    # ()
    tokens = [
        { position: { row: 1, column: 1 }, token: LParen },
        { position: { row: 1, column: 2 }, token: RParen },
    ]

    result = parse tokens

    result
    == Err [
        { position: { row: 1, column: 2 }, message: "Expected Name, found ')'" },
        { position: { row: 1, column: 2 }, message: "Unmatched ')'" },
    ]

expect
    # (10)
    tokens = [
        { position: { row: 1, column: 1 }, token: LParen },
        { position: { row: 1, column: 2 }, token: Number 10 },
        { position: { row: 1, column: 4 }, token: RParen },
    ]

    result = parse tokens

    result
    == Err [
        { message: "Expected Name, found 'Number 10'", position: { column: 2, row: 1 } },
        { message: "Unexpected Token Number 10", position: { column: 2, row: 1 } },
        { message: "Unmatched ')'", position: { column: 4, row: 1 } },
    ]

expect
    # )
    tokens = [
        { position: { row: 1, column: 1 }, token: RParen },
    ]

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

    result = parse tokens

    result
    == Err [
        {
            position: { row: 1, column: 1 },
            message: "Incomplete expression, no closing ')'",
        },
    ]

