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
        currentResult

transformMem : Transformer Mem
transformMem = \children ->
    Err [{ position: { row: 1, column: 1 }, message: "Not Implemented Yet" }]

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
    # (module (memory 1))
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
                        children: [Term { position: { column: 17, row: 1 }, term: Number 10 }],
                    },
                ],
            }
        )

    result
    == Ok {
        position: { row: 1, column: 1 },
        element: {
            funcs: [],
            mems: [],
            datas: [],
            imports: [],
            exports: [],
        },
    }

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
