module [
    transform,
    Module,
]

import Common
import Parser

TransformResult ok : Result ok (List Common.Error)
Transformer ok : List Parser.Expression -> TransformResult ok

MemIndex : U32
FuncIndex : Str

I32ConstInstruction : U32

Type : [I32]

FuncType : {
    identifier : Str,
    param : List (Common.Positionable Type),
    result : List (Common.Positionable Type),
}

Instruction : [
    I32Const (Common.Positionable U32),
    I32Store Common.Position,
    Call (Common.Positionable { identifier : Str }),
    Drop Common.Position,
]

Func : {
    identifier : Str,
    instructions : List Instruction,
}

Mem : {
    min : U32,
}

Data : {
    offset : List Instruction,
    bytes : Str,
}

Import : {
    namespace : Str,
    name : Str,
    definition : [Func Str],
}

# The result of `transformImport` is split into `types` and `imports` section
# on the module
TransformedImport : {
    namespace : Str,
    name : Str,
    definition : [Func FuncType],
}

Export : {
    name : Str,
    type : [Mem MemIndex, Func FuncIndex],
}

Module : {
    types : List (Common.Positionable FuncType),
    funcs : List (Common.Positionable Func),
    mems : List (Common.Positionable Mem),
    datas : List (Common.Positionable Data),
    imports : List (Common.Positionable Import),
    exports : List (Common.Positionable Export),
}

emptyModule = {
    types: [],
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

            SExpression { position, name: "export", children: [] } ->
                mergeError currentResult { position, message: "Invalid export: name and index required" }

            SExpression { position, name: "export", children: expressions } ->
                mergeResult currentResult (transformExport expressions) \module, export -> { module &
                        exports: List.append module.exports { position, element: export },
                    }

            SExpression { position, name: "data", children: [] } ->
                mergeError currentResult { position, message: "Invalid data: offset and bytes required" }

            SExpression { position, name: "data", children: expressions } ->
                mergeResult currentResult (transformData expressions) \module, data -> { module &
                        datas: List.append module.datas { position, element: data },
                    }

            SExpression { position, name: "func", children: [] } ->
                mergeError currentResult { position, message: "Invalid func: identifier required" }

            SExpression { position, name: "func", children: expressions } ->
                mergeResult currentResult (transformFunc expressions) \module, func -> { module &
                        funcs: List.append module.funcs { position, element: func },
                    }

            SExpression { position, name: "import", children: [] } ->
                mergeError currentResult { position, message: "Invalid import: namespace, name, and definition required" }

            SExpression { position, name: "import", children: expressions } ->
                mergeResult
                    currentResult
                    (transformImport expressions)
                    (\module, { name, namespace, definition: Func func } ->
                        { module &
                            types: List.append module.types { position, element: func },
                            imports: List.append module.imports { position, element: { name, namespace, definition: Func func.identifier } },
                        }
                    )

            _ -> Err [{ position: { row: 0, column: 0 }, message: "Unexpected child" }]

transformMem : Transformer Mem
transformMem = \children ->
    when children is
        [Term { term: Number number }] -> Ok { min: number }
        [Term { position }, ..] -> Err [{ position, message: "Invalid memory" }]
        [SExpression { position }, ..] -> Err [{ position, message: "Invalid memory" }]
        [] -> crash "transformMem is never called with an empty children"
        _ -> crash "Impossible match arm in transformMem" # shouldn't be necessary?

transformExport : Transformer Export
transformExport = \children ->
    when children is
        [Term { term: String name }, SExpression { name: "memory", children: [Term { term: Number memIndex }] }] ->
            Ok {
                name,
                type: Mem memIndex,
            }

        [Term { term: String name }, SExpression { name: "func", children: [Term { term: Variable funcIndex }] }] ->
            Ok {
                name,
                type: Func funcIndex,
            }

        [Term { term: String _ }, SExpression { position, name: exportType }] ->
            Err [{ position, message: "Can't handle export type $(exportType)" }]

        _ -> Err [{ position: { row: 0, column: 0 }, message: "Unexpected Export format" }]

transformData : Transformer Data
transformData = \children ->
    when children is
        [SExpression { name: "offset", children: offsetChildren }, Term { term: String string }] ->
            transformInstructions offsetChildren
            |> Result.map \instructions -> {
                offset: instructions,
                bytes: string,
            }

        _ -> Err [{ position: { row: 0, column: 0 }, message: "Unexpected data segment" }]

transformImport : Transformer TransformedImport
transformImport = \children ->
    when children is
        [Term { term: String namespace }, Term { term: String name }, SExpression { name: "func", children: funcChildren }] ->
            funcChildren
            |> transformFuncType
            |> Result.map \functionDefinition -> { namespace, name, definition: Func functionDefinition }

        _ -> Err [{ position: { row: 0, column: 0 }, message: "Unexpected import encountered" }]

transformFunc : Transformer Func
transformFunc = \children ->
    when children is
        [Term { term: Variable identifier }, .. as funcInstructions] ->
            funcInstructions
            |> transformInstructions
            |> Result.map \instructions -> {
                identifier,
                instructions,
            }

        _ -> Err [{ position: { row: 0, column: 0 }, message: "Unexpected Instruction" }]

transformInstructions : Transformer (List Instruction)
transformInstructions = \children ->
    List.walk
        children
        (Ok [])
        \currentResult, child ->
            when child is
                Term { position } ->
                    mergeError
                        currentResult
                        { position, message: "Expected instruction, got term" }

                SExpression { position, name: "i32.const", children: expressionChildren } ->
                    mergeResult
                        currentResult
                        (transformI32Const expressionChildren)
                        \currentList, element ->
                            List.append currentList (I32Const { position, element })

                SExpression { position, name: "i32.store", children: [] } ->
                    currentResult
                    |> Result.map \currentList -> List.append currentList (I32Store position)

                SExpression { position, name: "call", children: [Term { term: Variable identifier }] } ->
                    currentResult
                    |> Result.map \currentList -> List.append currentList (Call { position, element: { identifier } })

                SExpression { position, name: "drop", children: [] } ->
                    currentResult
                    |> Result.map \currentList -> List.append currentList (Drop position)

                SExpression { position, name } ->
                    mergeError
                        currentResult
                        { position, message: "Unexpected instruction $(name)" }

transformI32Const : Transformer I32ConstInstruction
transformI32Const = \children ->
    when children is
        [Term { term: Number value }] -> Ok value
        _ -> Err [{ position: { row: 0, column: 0 }, message: "Unexpected i32.const format. One number  expected." }]

transformTypes : Transformer (List (Common.Positionable Type))
transformTypes = \children ->
    List.walk children (Ok []) \currentResult, child ->
        when child is
            Term { position, term: Name "i32" } ->
                mergeResult
                    currentResult
                    (Ok { position, element: I32 })
                    List.append

            Term { position, term: Name name } ->
                mergeError
                    currentResult
                    { position, message: "Unexpected type: $(name)" }

            Term { position } ->
                mergeError
                    currentResult
                    { position, message: "Not a type" }

            SExpression { position } ->
                mergeError
                    currentResult
                    { position, message: "Not a type" }

            _ -> crash "Hit unexpected match arm in transformTypes" # Pretty sure this shouldn't be necessary

transformFuncType : Transformer FuncType
transformFuncType = \children ->
    when children is
        [Term { term: Variable identifier }, SExpression { name: "param", children: paramChildren }, SExpression { name: "result", children: resultChildren }] ->
            withParams = mergeResult
                (Ok { identifier, param: [], result: [] })
                (transformTypes paramChildren)
                \current, param -> { current &
                        param,
                    }
            mergeResult
                withParams
                (transformTypes resultChildren)
                \current, result -> { current &
                        result,
                    }

        _ -> Err [{ position: { row: 0, column: 0 }, message: "Inappropriate error handling in transformFuncType" }]

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
            types: [],
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

expect
    # (module
    #    (export "memory" (memory 0))
    # )
    result = transform
        (
            SExpression {
                position: { column: 1, row: 1 },
                name: "module",
                state: Closed,
                children: [
                    SExpression {
                        position: { column: 4, row: 2 },
                        name: "export",
                        state: Closed,
                        children: [
                            Term {
                                position: { column: 15, row: 2 },
                                term: String "memory",
                            },
                            SExpression {
                                position: { column: 21, row: 2 },
                                name: "memory",
                                state: Closed,
                                children: [
                                    Term {
                                        position: { column: 30, row: 2 },
                                        term: Number 0,
                                    },
                                ],
                            },
                        ],
                    },
                ],
            }
        )

    result
    ==
    Ok {
        position: { row: 1, column: 1 },
        element: {
            types: [],
            funcs: [],
            mems: [],
            datas: [],
            imports: [],
            exports: [
                {
                    position: { row: 2, column: 4 },
                    element: {
                        name: "memory",
                        type: Mem 0,
                    },
                },
            ],
        },
    }

expect
    # (module
    #    (export "myfunc" (func $identifier))
    # )
    result = transform
        (
            SExpression {
                position: { column: 1, row: 1 },
                name: "module",
                state: Closed,
                children: [
                    SExpression {
                        position: { column: 4, row: 2 },
                        name: "export",
                        state: Closed,
                        children: [
                            Term {
                                position: { column: 15, row: 2 },
                                term: String "myfunc",
                            },
                            SExpression {
                                position: { column: 21, row: 2 },
                                name: "func",
                                state: Closed,
                                children: [
                                    Term {
                                        position: { column: 30, row: 2 },
                                        term: Variable "identifier",
                                    },
                                ],
                            },
                        ],
                    },
                ],
            }
        )

    result
    ==
    Ok {
        position: { row: 1, column: 1 },
        element: {
            types: [],
            funcs: [],
            mems: [],
            datas: [],
            imports: [],
            exports: [
                {
                    position: { row: 2, column: 4 },
                    element: {
                        name: "myfunc",
                        type: Func "identifier",
                    },
                },
            ],
        },
    }

expect
    # (module
    #   (data (offset (i32.const 8)) "hello world")
    # )
    result = transform
        (
            SExpression {
                position: { column: 1, row: 1 },
                name: "module",
                state: Closed,
                children: [
                    SExpression {
                        position: { column: 3, row: 2 },
                        name: "data",
                        state: Closed,
                        children: [
                            SExpression {
                                position: { column: 9, row: 2 },
                                name: "offset",
                                state: Closed,
                                children: [
                                    SExpression {
                                        position: { column: 17, row: 2 },
                                        name: "i32.const",
                                        state: Closed,
                                        children: [
                                            Term {
                                                position: { column: 28, row: 2 },
                                                term: Number 8,
                                            },
                                        ],
                                    },
                                ],
                            },
                            Term {
                                position: { row: 1, column: 30 },
                                term: String "hello world",
                            },
                        ],
                    },
                ],
            }
        )

    result
    ==
    Ok {
        position: { row: 1, column: 1 },
        element: {
            types: [],
            funcs: [],
            mems: [],
            datas: [
                {
                    position: { row: 2, column: 3 },
                    element: {
                        offset: [I32Const { position: { row: 2, column: 17 }, element: 8 }],
                        bytes: "hello world",
                    },
                },
            ],
            imports: [],
            exports: [],
        },
    }

expect
    # (...i32, i32)
    result = transformTypes [
        Term { position: { row: 1, column: 5 }, term: Name "i32" },
        Term { position: { row: 1, column: 9 }, term: Name "i32" },
    ]

    result
    ==
    Ok [
        { element: I32, position: { column: 5, row: 1 } },
        { element: I32, position: { column: 9, row: 1 } },
    ]

expect
    # (...f32) isn't supported yet
    result = transformTypes [
        Term { position: { row: 1, column: 5 }, term: Name "f32" },
    ]

    result == Err [{ position: { column: 5, row: 1 }, message: "Unexpected type: f32" }]

expect
    # (... $identifier (param i32 i32) (result i32))
    result = transformFuncType [
        Term { position: { row: 1, column: 5 }, term: Variable "$identifier" },
        SExpression {
            position: { row: 1, column: 17 },
            name: "param",
            state: Closed,
            children: [
                Term { position: { row: 1, column: 24 }, term: Name "i32" },
                Term { position: { row: 1, column: 28 }, term: Name "i32" },
            ],
        },
        SExpression {
            position: { row: 1, column: 33 },
            name: "result",
            state: Closed,
            children: [
                Term { position: { row: 1, column: 41 }, term: Name "i32" },
            ],
        },
    ]

    result
    == Ok {
        identifier: "$identifier",
        param: [
            { position: { row: 1, column: 24 }, element: I32 },
            { position: { row: 1, column: 28 }, element: I32 },
        ],
        result: [
            { position: { row: 1, column: 41 }, element: I32 },
        ],
    }

expect
    # (module
    #   (func $myfunc
    #     (i32.const 8)
    #     (i32.const 2)
    #     (i32.store)
    #   )
    # )
    result = transform
        (
            SExpression {
                position: { column: 1, row: 1 },
                name: "module",
                state: Closed,
                children: [
                    SExpression {
                        position: { column: 3, row: 2 },
                        name: "func",
                        state: Closed,
                        children: [
                            Term {
                                position: { row: 2, column: 9 },
                                term: Variable "myfunc",
                            },
                            SExpression {
                                position: { column: 6, row: 3 },
                                name: "i32.const",
                                state: Closed,
                                children: [
                                    Term {
                                        position: { row: 3, column: 13 },
                                        term: Number 8,
                                    },
                                ],
                            },
                            SExpression {
                                position: { column: 6, row: 4 },
                                name: "i32.const",
                                state: Closed,
                                children: [
                                    Term {
                                        position: { row: 4, column: 13 },
                                        term: Number 2,
                                    },
                                ],
                            },
                            SExpression {
                                position: { column: 6, row: 5 },
                                name: "i32.store",
                                state: Closed,
                                children: [],
                            },
                        ],
                    },
                ],
            }
        )

    result
    ==
    Ok {
        element: {
            datas: [],
            exports: [],
            funcs: [
                {
                    element: {
                        identifier: "myfunc",
                        instructions: [
                            I32Const { element: 8, position: { column: 6, row: 3 } },
                            I32Const { element: 2, position: { column: 6, row: 4 } },
                            I32Store { column: 6, row: 5 },
                        ],
                    },
                    position: { column: 3, row: 2 },
                },
            ],
            imports: [],
            mems: [],
            types: [],
        },
        position: { column: 1, row: 1 },
    }

expect
    # (module
    #   (import
    #     "wasi_snapshot_preview1"
    #     "fd_write"
    #     (func $fd_write (param i32 i32 i32 i32) (result i32))
    #   )
    # )
    result = transform
        (
            SExpression {
                position: { column: 1, row: 1 },
                name: "module",
                state: Closed,
                children: [
                    SExpression {
                        position: { column: 3, row: 2 },
                        name: "import",
                        state: Closed,
                        children: [
                            Term {
                                position: { row: 3, column: 4 },
                                term: String "wasi_snapshot_preview1",
                            },
                            Term {
                                position: { row: 3, column: 4 },
                                term: String "fd_write",
                            },
                            SExpression {
                                position: { column: 4, row: 5 },
                                name: "func",
                                state: Closed,
                                children: [
                                    Term {
                                        position: { row: 5, column: 10 },
                                        term: Variable "fd_write",
                                    },
                                    SExpression {
                                        position: { column: 20, row: 5 },
                                        name: "param",
                                        state: Closed,
                                        children: [
                                            Term {
                                                position: { column: 27, row: 5 },
                                                term: Name "i32",
                                            },
                                            Term {
                                                position: { column: 31, row: 5 },
                                                term: Name "i32",
                                            },
                                            Term {
                                                position: { column: 35, row: 5 },
                                                term: Name "i32",
                                            },
                                            Term {
                                                position: { column: 39, row: 5 },
                                                term: Name "i32",
                                            },
                                        ],
                                    },
                                    SExpression {
                                        position: { column: 44, row: 5 },
                                        name: "result",
                                        state: Closed,
                                        children: [
                                            Term {
                                                position: { column: 51, row: 5 },
                                                term: Name "i32",
                                            },
                                        ],
                                    },
                                ],
                            },
                        ],
                    },
                ],
            }
        )

    result
    ==
    Ok {
        position: { column: 1, row: 1 },
        element: {
            datas: [],
            exports: [],
            funcs: [],
            imports: [
                {
                    position: { column: 3, row: 2 },
                    element: {
                        definition: Func "fd_write",
                        name: "fd_write",
                        namespace: "wasi_snapshot_preview1",
                    },
                },
            ],
            mems: [],
            types: [
                {
                    position: { column: 3, row: 2 },
                    element: {
                        identifier: "fd_write",
                        param: [
                            {
                                element: I32,
                                position: { column: 27, row: 5 },
                            },
                            {
                                element: I32,
                                position: { column: 31, row: 5 },
                            },
                            {
                                element: I32,
                                position: { column: 35, row: 5 },
                            },
                            {
                                element: I32,
                                position: { column: 39, row: 5 },
                            },
                        ],
                        result: [
                            {
                                element: I32,
                                position: { column: 51, row: 5 },
                            },
                        ],
                    },
                },
            ],
        },
    }
