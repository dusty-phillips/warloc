module [
    Token,
    tokenize,
    toStr,
]

import Common

TokenTag : [LParen, RParen, Name Str, Number U32, String Str, Variable Str]
Token : {
    position : Common.Position,
    token : TokenTag,
}
CurrentToken : {
    position : Common.Position,
    token : [None, Name (List U8), Number (List U8), String (List U8), Variable (List U8)],
}

TokenizerState : {
    currentToken : CurrentToken,
    currentPosition : Common.Position,
    tokens : List Token,
    errors : List Common.Error,
}

tokenize : Str -> Result (List Token) (List Common.Error)
tokenize = \input ->
    finalState = Str.walkUtf8
        input
        initialState
        evaluateOneChar

    when finalState.errors is
        [] -> Ok finalState.tokens
        errors -> Err errors

initialState : TokenizerState
initialState = {
    currentToken: {
        position: {
            row: 1,
            column: 0,
        },
        token: None,
    },
    currentPosition: {
        row: 1,
        column: 0,
    },
    tokens: [],
    errors: [],
}

evaluateOneChar : TokenizerState, U8 -> TokenizerState
evaluateOneChar = \previousState, nextByte ->

    currentState =
        if nextByte == '\n' then
            { previousState &
                currentPosition: {
                    row: previousState.currentPosition.row + 1,
                    column: 0,
                },
            }
        else
            { previousState &
                currentPosition: {
                    row: previousState.currentPosition.row,
                    column: previousState.currentPosition.column + 1,
                },
            }

    when (currentState.currentToken.token, nextByte) is
        (None, '(') ->
            { currentState &
                tokens: List.append currentState.tokens {
                    position: currentState.currentPosition,
                    token: LParen,
                },
            }

        (None, ')') ->
            { currentState &
                tokens: List.append currentState.tokens {
                    position: currentState.currentPosition,
                    token: RParen,
                },
            }

        (None, whitespaceByte) if Set.contains whitespaceBytes whitespaceByte ->
            currentState

        (None, nameByte) if Set.contains validNameStartBytes nameByte ->
            { currentState &
                currentToken: {
                    position: currentState.currentPosition,
                    token: Name [nameByte],
                },
            }

        (None, numberByte) if Set.contains validDigitBytes numberByte ->
            { currentState &
                currentToken: {
                    position: currentState.currentPosition,
                    token: Number [numberByte],
                },
            }

        (None, '"') ->
            { currentState &
                currentToken: {
                    position: currentState.currentPosition,
                    token: String [],
                },
            }

        (None, '$') ->
            { currentState &
                currentToken: {
                    position: currentState.currentPosition,
                    token: Variable [],
                },
            }

        (Name nameBytes, nameByte) if Set.contains validNameBytes nameByte ->
            currentToken = currentState.currentToken
            { currentState &
                currentToken: { currentToken &
                    token: Name (List.append nameBytes nameByte),
                },
            }

        (Name nameBytes, whitespaceByte) if Set.contains whitespaceBytes whitespaceByte ->
            endString currentState nameBytes Name

        (Name nameBytes, ')') ->
            withStrState = endString currentState nameBytes Name
            { withStrState &
                tokens: List.append withStrState.tokens {
                    position: withStrState.currentPosition,
                    token: RParen,
                },
            }

        (Number numberBytes, numberByte) if Set.contains validDigitBytes numberByte ->
            currentToken = currentState.currentToken
            { currentState &
                currentToken: { currentToken &
                    token: Number (List.append numberBytes numberByte),
                },
            }

        (Number numberBytes, whitespaceByte) if Set.contains whitespaceBytes whitespaceByte ->
            { currentState &
                currentToken: {
                    position: currentState.currentPosition,
                    token: None,
                },
                tokens: List.append currentState.tokens {
                    position: currentState.currentToken.position,
                    token: Number (numberBytes |> digitsToNumber),
                },
            }

        (Number numberBytes, ')') ->
            { currentState &
                currentToken: {
                    position: currentState.currentToken.position,
                    token: None,
                },
                tokens: List.concat currentState.tokens [
                    {
                        position: currentState.currentToken.position,
                        token: Number (numberBytes |> digitsToNumber),
                    },
                    {
                        position: currentState.currentPosition,
                        token: RParen,
                    },
                ],
            }

        (String stringBytes, '"') ->
            endString currentState stringBytes String

        (String stringBytes, stringByte) ->
            currentToken = currentState.currentToken
            { currentState &
                currentToken: { currentToken &
                    token: String (List.append stringBytes stringByte),
                },
            }

        (Variable variableBytes, variableByte) if Set.contains validNameBytes variableByte ->
            currentToken = currentState.currentToken
            { currentState &
                currentToken: { currentToken &
                    token: Variable (List.append variableBytes variableByte),
                },
            }

        (Variable variableBytes, whitespaceByte) if Set.contains whitespaceBytes whitespaceByte ->
            endString currentState variableBytes Variable

        (Variable variableBytes, ')') ->
            withStrState = endString currentState variableBytes Variable
            { withStrState &
                tokens: List.append withStrState.tokens {
                    position: withStrState.currentPosition,
                    token: RParen,
                },
            }

        (Variable _, unexpectedByte) | (Number _, unexpectedByte) | (Name _, unexpectedByte) | (None, unexpectedByte) ->
            byteAsUtf8 =
                [unexpectedByte]
                |> Str.fromUtf8
                |> Result.onErr \_ -> Err (Num.toStr unexpectedByte)
                |> Result.withDefault ""

            { currentState &
                errors: List.append currentState.errors {
                    message: "Unexpected character '$(byteAsUtf8)'",
                    position: currentState.currentPosition,
                },
            }

digitsToNumber : List U8 -> U32
digitsToNumber = \digits ->
    digits
    |> List.walk 0u32 \state, digit ->

        expect
            digit >= 48

        expect
            digit <= 48 + 9

        (state * 10u32) + ((digit - 48) |> Num.toU32)

expect digitsToNumber [] == 0
expect digitsToNumber [48u8] == 0
expect digitsToNumber [49u8] == 1
expect digitsToNumber [49u8, 48u8] == 10
expect digitsToNumber [49u8, 52u8, 55u8] == 147
expect digitsToNumber [48u8, 48u8, 49u8, 52u8, 55u8] == 147

endString : TokenizerState, List U8, (Str -> TokenTag) -> TokenizerState
endString = \currentState, nameBytes, makeToken ->
    when Str.fromUtf8 nameBytes is
        Err _ ->
            bytesDisplay = nameBytes |> List.map Num.toStr |> Str.joinWith ', '
            { currentState &
                errors: List.append currentState.errors {
                    message: "Unexpected Utf8 byte in: $(bytesDisplay)",
                    position: currentState.currentPosition,
                },
            }

        Ok string ->
            { currentState &
                currentToken: {
                    position: currentState.currentToken.position,
                    token: None,
                },
                tokens: List.append currentState.tokens {
                    position: currentState.currentToken.position,
                    token: makeToken string,
                },
            }

validNameStartBytes = Set.fromList [
    'a',
    'b',
    'c',
    'd',
    'e',
    'f',
    'g',
    'h',
    'i',
    'j',
    'k',
    'l',
    'm',
    'n',
    'o',
    'p',
    'q',
    'r',
    's',
    't',
    'u',
    'v',
    'w',
    'x',
    'y',
    'z',
]

validDigitBytes =
    Set.fromList [
        '0',
        '1',
        '2',
        '3',
        '4',
        '5',
        '6',
        '7',
        '8',
        '9',
    ]

validNameBytes =
    validNameStartBytes
    |> Set.union validDigitBytes
    |> Set.union
        (
            Set.fromList [
                '.',
                '_',
            ]
        )

whitespaceBytes = Set.fromList [9u8, 10u8, 13u8, 32u8]

toStr : Token -> Str
toStr = \token ->
    when token.token is
        LParen -> "("
        RParen -> ")"
        Name name -> "Name $(name)"
        Number number -> "Number $(number |> Num.toStr)"
        String string -> "String $(string)"
        Variable variable -> "Variable $(variable)"

expect
    tokens = tokenize "(module)"

    tokens
    == Ok [
        { position: { row: 1, column: 1 }, token: LParen },
        { position: { row: 1, column: 2 }, token: Name "module" },
        { position: { row: 1, column: 8 }, token: RParen },
    ]

expect
    result = tokenize "(module!)"

    result == Err [{ position: { row: 1, column: 8 }, message: "Unexpected character '!'" }]

expect
    tokens = tokenize "(\n  module\n)"

    tokens
    == Ok [
        { position: { row: 1, column: 1 }, token: LParen },
        { position: { row: 2, column: 3 }, token: Name "module" },
        { position: { row: 3, column: 1 }, token: RParen },
    ]

expect
    result = tokenize "(!\n  module!\n)"

    result
    == Err [
        { position: { row: 1, column: 2 }, message: "Unexpected character '!'" },
        { position: { row: 2, column: 9 }, message: "Unexpected character '!'" },
    ]

expect
    result = tokenize
        """
        (
          module
          (memory 10)
        )
        """

    result
    == Ok [
        { position: { row: 1, column: 1 }, token: LParen },
        { position: { row: 2, column: 3 }, token: Name "module" },
        { position: { row: 3, column: 3 }, token: LParen },
        { position: { row: 3, column: 4 }, token: Name "memory" },
        { position: { row: 3, column: 11 }, token: Number 10 },
        { position: { row: 3, column: 13 }, token: RParen },
        { position: { row: 4, column: 1 }, token: RParen },
    ]

expect
    result = tokenize
        """
        (
          module
          (memory 10 )
        )
        """

    result
    == Ok [
        { position: { row: 1, column: 1 }, token: LParen },
        { position: { row: 2, column: 3 }, token: Name "module" },
        { position: { row: 3, column: 3 }, token: LParen },
        { position: { row: 3, column: 4 }, token: Name "memory" },
        { position: { row: 3, column: 11 }, token: Number 10 },
        { position: { row: 3, column: 14 }, token: RParen },
        { position: { row: 4, column: 1 }, token: RParen },
    ]

expect
    result = tokenize
        """
        (module
          (export "memory" (memory 0))
        )
        """

    result
    == Ok [
        { position: { row: 1, column: 1 }, token: LParen },
        { position: { row: 1, column: 2 }, token: Name "module" },
        { position: { row: 2, column: 3 }, token: LParen },
        { position: { row: 2, column: 4 }, token: Name "export" },
        { position: { row: 2, column: 11 }, token: String "memory" },
        { position: { row: 2, column: 20 }, token: LParen },
        { position: { row: 2, column: 21 }, token: Name "memory" },
        { position: { row: 2, column: 28 }, token: Number 0 },
        { position: { row: 2, column: 29 }, token: RParen },
        { position: { row: 2, column: 30 }, token: RParen },
        { position: { row: 3, column: 1 }, token: RParen },
    ]

expect
    result = tokenize
        """
        (module
          (export "memory")
        )
        """

    result
    == Ok [
        { position: { row: 1, column: 1 }, token: LParen },
        { position: { row: 1, column: 2 }, token: Name "module" },
        { position: { row: 2, column: 3 }, token: LParen },
        { position: { row: 2, column: 4 }, token: Name "export" },
        { position: { row: 2, column: 11 }, token: String "memory" },
        { position: { row: 2, column: 19 }, token: RParen },
        { position: { row: 3, column: 1 }, token: RParen },
    ]

expect
    result = tokenize
        """
        (module
          (func $main)
        )
        """

    result
    == Ok [
        { position: { row: 1, column: 1 }, token: LParen },
        { position: { row: 1, column: 2 }, token: Name "module" },
        { position: { row: 2, column: 3 }, token: LParen },
        { position: { row: 2, column: 4 }, token: Name "func" },
        { position: { row: 2, column: 9 }, token: Variable "main" },
        { position: { row: 2, column: 14 }, token: RParen },
        { position: { row: 3, column: 1 }, token: RParen },
    ]
