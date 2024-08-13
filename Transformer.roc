module [
    transform,
    Module,
]

import Common
import Parser

TransformResult ok : Result ok (List Common.Error)
Transformer ok : List Parser.Expression -> TransformResult ok

Func : {}
Mem : {
    min : U32,
}
Data : {}
Import : {}
Export : {}

Module : {
    funcs : List (Common.Positionable Func),
    mems : List (Common.Positionable Mem),
    datas : List (Common.Positionable Data),
    imports : List (Common.Positionable Import),
    exports : List (Common.Positionable Export),
}

emptyModule = {
    funcs: [],
    mems: [],
    datas: [],
    imports: [],
    exports: [],
}

transform : Parser.Expression -> TransformResult (Common.Positionable Module)
transform = \expression ->
    when expression is
        SExpression { position, name: "module", children } ->
            children
            |> transformModule
            |> Result.map \element -> { position, element }

        SExpression { position, name } ->
            Err [
                {
                    position,
                    message: "Expected a 'module', but received a '$(name)'",
                },
            ]

        Term { position } ->
            Err [
                {
                    position,
                    message: "Expected a S-expression, but received a Term",
                },
            ]

transformModule : Transformer Module
transformModule = \children ->
    List.walk children (Ok emptyModule) \currentResult, child ->
        when child is
            SExpression { position, name: "memory", children: [] } ->
                mergeError currentResult { position, message: "Invalid memory: min limit required" }

            SExpression { position, name: "memory", children: expressions } ->
                mergeResult currentResult (transformMem expressions) \module, mem -> { module &
                        mems: List.append module.mems { position, element: mem },
                    }

            _ -> Err [{ position: { row: 0, column: 0 }, message: "Unexpected child" }]
# when memResult and currentResult are errors, merge the errors
# when memResult is succes and currentResult is error, new errors list
# when memResult is error and currentResult is success, new error list
# when memResult is success and currentResult is success, merge with custom function

transformMem : Transformer Mem
transformMem = \children ->
    when children is
        [Term { position, term: Number number }] -> Ok { min: number }
        [Term { position }, ..] -> Err [{ position, message: "Invalid memory" }]
        [SExpression { position }, ..] -> Err [{ position, message: "Invalid memory" }]
        [] -> crash "transformMem is never called with an empty children"
        _ -> crash "Impossible match arm in transformMem" # shouldn't be necessary?

mergeResult : TransformResult a, TransformResult b, (a, b -> a) -> TransformResult a
mergeResult = \originalResult, nextResult, mapper ->

    when (originalResult, nextResult) is
        (Ok original, Ok next) ->
            Ok (mapper original next)

        (Err errors, Ok _) ->
            Err errors

        (Ok _, Err errors) ->
            Err errors

        (Err originalErrors, Err nextErrors) ->
            Err (List.concat originalErrors nextErrors)

mergeError : TransformResult a, Common.Error -> TransformResult a
mergeError = \originalResult, error ->
    when originalResult is
        Ok _ ->
            errors = [error]
            Err errors

        Err errors -> Err (List.append errors error)

expect
    # (module)
    result = transform
        (
            SExpression {
                position: { row: 1, column: 1 },
                name: "module",
                children: [],
                state: Closed,
            }
        )

    result
    == Ok {
        position: { row: 1, column: 1 },
        element: emptyModule,
    }

expect
    # (module (memory 10))
    result = transform
        (
            SExpression {
                position: { column: 1, row: 1 },
                name: "module",
                state: Closed,
                children: [
                    SExpression {
                        position: { column: 8, row: 1 },
                        name: "memory",
                        state: Closed,
                        children: [
                            Term {
                                position: { column: 17, row: 1 },
                                term: Number 10,
                            },
                        ],
                    },
                ],
            }
        )

    result
    == Ok {
        position: { row: 1, column: 1 },
        element: {
            funcs: [],
            mems: [
                {
                    position: { row: 1, column: 8 },
                    element: { min: 10u32 },
                },
            ],
            datas: [],
            imports: [],
            exports: [],
        },
    }

expect
    # (module (memory))
    result = transform
        (
            SExpression {
                position: { column: 1, row: 1 },
                name: "module",
                state: Closed,
                children: [
                    SExpression {
                        position: { column: 8, row: 1 },
                        name: "memory",
                        state: Closed,
                        children: [],
                    },
                ],
            }
        )

    result
    == Err [{ message: "Invalid memory: min limit required", position: { column: 8, row: 1 } }]

expect
    # (module (memory "ten"))
    result = transform
        (
            SExpression {
                position: { column: 1, row: 1 },
                name: "module",
                state: Closed,
                children: [
                    SExpression {
                        position: { column: 8, row: 1 },
                        name: "memory",
                        state: Closed,
                        children: [
                            Term {
                                position: { column: 17, row: 1 },
                                term: String "10",
                            },
                        ],
                    },
                ],
            }
        )

    result
    == Err [{ message: "Invalid memory", position: { column: 17, row: 1 } }]

expect
    # (module (memory (something 10)))
    result = transform
        (
            SExpression {
                position: { column: 1, row: 1 },
                name: "module",
                state: Closed,
                children: [
                    SExpression {
                        position: { column: 8, row: 1 },
                        name: "memory",
                        state: Closed,
                        children: [
                            SExpression {
                                position: { column: 17, row: 1 },
                                name: "something",
                                state: Closed,
                                children: [
                                    Term {
                                        position: { column: 28, row: 1 },
                                        term: Number 10,
                                    },
                                ],
                            },
                        ],
                    },
                ],
            }
        )

    result
    == Err [{ message: "Invalid memory", position: { column: 17, row: 1 } }]

expect
    # (memory)
    result = transform
        (
            SExpression {
                position: { row: 1, column: 1 },
                name: "memory",
                children: [],
                state: Closed,
            }
        )

    result
    == Err [
        {
            message: "Expected a 'module', but received a 'memory'",
            position: { column: 1, row: 1 },
        },
    ]

expect
    # 42
    # (technically, parser would never emit this)
    result = transform
        (
            Term {
                position: { row: 1, column: 1 },
                term: Number 42,
            }
        )

    result
    == Err [
        {
            message: "Expected a S-expression, but received a Term",
            position: { column: 1, row: 1 },
        },
    ]
