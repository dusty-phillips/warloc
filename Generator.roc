module [
    generate,
]

import Transformer
import Common

generate : Common.Positionable Transformer.Module -> List U8
generate = \modulePositioned ->
    module = modulePositioned |> Common.extractElement

    [0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00]
    |> concatSection 0x01 module.types generateFuncType
    |> concatSection 0x02 module.imports (generateImportFactory module.types)
    |> concatSection 0x03 module.funcs (generateFuncFactory module.types)
    |> concatSection 0x05 module.mems generateMem
    |> concatSection 0x0a module.funcs generateCode
    |> concatSection 0x0b module.datas generateData

concatSection : List U8, U8, List (Common.Positionable x), (x -> List U8) -> List U8
concatSection = \existingBytes, sectionId, items, generateOne ->
    sectionBytes =
        items
        |> List.map Common.extractElement
        |> generateSection sectionId generateOne

    List.concat existingBytes sectionBytes

generateSection : List a, U8, (a -> List U8) -> List U8
generateSection = \items, sectionId, generateOne ->
    when items is
        [] -> []
        _ ->
            section = generateVector items generateOne

            []
            |> List.append sectionId
            |> List.concat (section |> List.len |> Num.toU32 |> generateU32)
            |> List.concat section

generateImportFactory : List (Common.Positionable Transformer.FuncType) -> (Transformer.Import -> List U8)
generateImportFactory = \funcTypes ->
    typeDict = typesToDict funcTypes

    \importDef ->
        when importDef.definition is
            Func identifier if Dict.contains typeDict identifier ->
                []
                |> List.concat (generateVector (Str.toUtf8 importDef.namespace) \b -> [b])
                |> List.concat (generateVector (Str.toUtf8 importDef.name) \b -> [b])
                |> List.append 0x00
                |> List.concat (generateU32 ((Dict.get typeDict identifier) |> Result.withDefault 999999))

            Func _ ->
                crash "Unexpected identifier shouldn't get past AST generation"

            _ ->
                crash "Haven't implemented non func imports"

generateFuncFactory : List (Common.Positionable Transformer.FuncType) -> (Transformer.Func -> List U8)
generateFuncFactory = \funcTypes ->
    typeDict = typesToDict funcTypes

    \func ->
        when func.identifier is
            identifier if Dict.contains typeDict identifier ->
                typeDict
                |> Dict.get func.identifier
                |> Result.withDefault 999999
                |> generateU32

            _ ->
                crash "Unexpected identifier shouldn't get past AST generation"

generateMem : Transformer.Mem -> List U8
generateMem = \mem ->
    List.concat [0x00] (generateU32 mem.min)

generateCode : Transformer.Func -> List U8
generateCode = \func ->
    bytes =
        [0x00] # hardcoding the locals to be empty for now
        |> List.concat (generateExpression func.instructions)

    bytes
    |> List.len
    |> Num.toU32
    |> generateU32
    |> List.concat bytes

generateData : Transformer.Data -> List U8
generateData = \data ->
    [0x00]
    |> List.concat (generateExpression data.offset)
    |> List.concat (generateVector (Str.toUtf8 data.bytes) \b -> [b])

generateInstruction : Transformer.Instruction -> List U8
generateInstruction = \instruction ->
    when instruction is
        I32Const { element } -> List.concat [0x41] (element |> generateU32)
        I32Store {} -> [0x36, 0x02, 0x00]
        Drop {} -> [0x1a]
        _ -> crash "unexpected instruction encountered"

generateExpression : List Transformer.Instruction -> List U8
generateExpression = \instructions ->
    instructions
    |> List.walk
        []
        (\state, next ->
            List.concat state (generateInstruction next)
        )
    |> List.append 0x0b

generateU32 : U32 -> List U8
generateU32 = \number ->
    generateU32Recurse [] number

generateU32Recurse : List U8, U32 -> List U8
generateU32Recurse = \currentItems, remainingBits ->
    leastSignificantByte = Num.bitwiseAnd remainingBits 0x7f
    nextBits = Num.shiftRightBy remainingBits 7

    if (nextBits == 0) && ((Num.bitwiseAnd leastSignificantByte 0x40) == 0) then
        List.append currentItems (Num.toU8 leastSignificantByte)
    else
        generateU32Recurse (List.append currentItems (Num.toU8 (Num.bitwiseOr leastSignificantByte 0x80))) nextBits

generateVector : List a, (a -> List U8) -> List U8
generateVector = \items, encodeOne ->
    result = items |> List.len |> Num.toU32 |> generateU32
    items
    |> List.walk result \current, next ->
        List.concat current (encodeOne next)

generateType : Transformer.Type -> List U8
generateType = \type ->
    when type is
        I32 -> [0x7f]

generateFuncType : Transformer.FuncType -> List U8
generateFuncType = \funcType ->
    []
    |> List.append 0x60
    |> List.concat (generateVector (funcType.param |> List.map Common.extractElement) generateType)
    |> List.concat (generateVector (funcType.result |> List.map Common.extractElement) generateType)

typesToDict : List (Common.Positionable Transformer.FuncType) -> Dict Str U32
typesToDict = \funcTypes ->
    funcTypes
    |> List.map Common.extractElement
    |> List.mapWithIndex \funcType, index -> (funcType.identifier, index |> Num.toU32)
    |> Dict.fromList

expect
    # (module)
    result = generate {
        position: { row: 1, column: 1 },
        element: {
            types: [],
            funcs: [],
            mems: [],
            datas: [],
            imports: [],
            exports: [],
        },
    }

    result == [0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00]

expect
    # (module
    #   (type (
    #     func (param i32 i32 i32 i32) (result i32))
    #   )
    # )
    result = generate {
        position: { row: 1, column: 1 },
        element: {
            types: [
                {
                    position: { row: 2, column: 3 },
                    element: {
                        identifier: "helloFunc",
                        param: [
                            { element: I32, position: { row: 1, column: 1 } },
                            { element: I32, position: { row: 1, column: 1 } },
                        ],
                        result: [{ element: I32, position: { row: 1, column: 1 } }],
                    },
                },
            ],
            funcs: [],
            mems: [],
            datas: [],
            imports: [],
            exports: [],
        },
    }

    result
    == [
        0x00,
        0x61,
        0x73,
        0x6d,
        0x01,
        0x00,
        0x00,
        0x00,
        0x01,
        0x07,
        0x01,
        0x60,
        0x02,
        0x7f,
        0x7f,
        0x01,
        0x7f,
    ]

expect
    # (module
    #     (import "wasi_snapshot_preview1" "fd_write" (
    #         func $fd_write (param i32 i32 i32 i32) (result i32))
    #     )
    # )
    result = generate {
        position: { row: 1, column: 1 },
        element: {
            types: [
                {
                    position: { row: 3, column: 48 },
                    element: {
                        identifier: "fd_write",
                        param: [
                            { element: I32, position: { row: 3, column: 30 } },
                            { element: I32, position: { row: 3, column: 34 } },
                            { element: I32, position: { row: 3, column: 38 } },
                            { element: I32, position: { row: 3, column: 42 } },
                        ],
                        result: [{ element: I32, position: { row: 3, column: 55 } }],
                    },
                },
            ],
            funcs: [],
            mems: [],
            datas: [],
            imports: [
                {
                    position: { row: 2, column: 4 },
                    element: {
                        namespace: "wasi_snapshot_preview1",
                        name: "fd_write",
                        definition: Func "fd_write",
                    },
                },
            ],
            exports: [],
        },
    }

    result
    == [
        0x00,
        0x61,
        0x73,
        0x6d,
        0x01,
        0x00,
        0x00,
        0x00,
        0x01, # type section 1
        0x09,
        0x01,
        0x60,
        0x04,
        0x7f,
        0x7f,
        0x7f,
        0x7f,
        0x01,
        0x7f,
        0x02, # import section 2
        0x23,
        0x01, # One import
        0x16, # namespace length
        0x77,
        0x61,
        0x73,
        0x69,
        0x5f,
        0x73,
        0x6e,
        0x61,
        0x70,
        0x73,
        0x68,
        0x6f,
        0x74,
        0x5f,
        0x70,
        0x72,
        0x65,
        0x76,
        0x69,
        0x65,
        0x77,
        0x31,
        0x08, # name length
        0x66,
        0x64,
        0x5f,
        0x77,
        0x72,
        0x69,
        0x74,
        0x65,
        0x00, # Func type
        0x00, # Func index
    ]

expect
    # (module
    #     (func $main
    #         (i32.const 0)
    #         (i32.const 8)
    #         (i32.store)
    #     )
    # )
    result = generate {
        position: { row: 1, column: 1 },
        element: {
            types: [
                {
                    position: { row: 2, column: 4 },
                    element: {
                        identifier: "main",
                        param: [],
                        result: [],
                    },
                },
            ],
            funcs: [
                {
                    position: { row: 2, column: 4 },
                    element: {
                        identifier: "main",
                        instructions: [
                            I32Const {
                                position: { row: 3, column: 8 },
                                element: 0,
                            },
                            I32Const {
                                position: { row: 4, column: 8 },
                                element: 8,
                            },
                            I32Store { row: 5, column: 8 },
                        ],
                    },
                },
            ],
            mems: [],
            datas: [],
            imports: [],
            exports: [],
        },
    }

    result
    ==
    [
        0x00,
        0x61,
        0x73,
        0x6d,
        0x01,
        0x00,
        0x00,
        0x00,
        0x01, # type section
        0x04,
        0x01,
        0x60,
        0x00,
        0x00,
        0x03, # func section
        0x02, # two bytes
        0x01, # 1 func
        0x00, # func is at index 0 in typeindex
        0x0a, # code section
        0x0b, # 11 bytes
        0x01, # 1 function
        0x09, # nine bytes in this function
        0x00, # 0 local blocks
        0x41, # i32_const
        0x00,
        0x41, # i32_const
        0x08,
        0x36, # i32_store with hardcoded alignment/offset
        0x02,
        0x00,
        0x0b,
    ]

expect
    # (module
    #    (memory 1)
    # )
    result = generate {
        position: { row: 1, column: 1 },
        element: {
            types: [],
            funcs: [],
            mems: [{ position: { row: 2, column: 4 }, element: { min: 1 } }],
            datas: [],
            imports: [],
            exports: [],
        },
    }

    result
    == [
        0x00,
        0x61,
        0x73,
        0x6d,
        0x01,
        0x00,
        0x00,
        0x00, # header
        0x05, # memory section
        0x03,
        0x01, # vector with 1 element
        0x00,
        0x01,
    ]

expect
    # (module
    #   (data (offset (i32.const 8)) "hello world")
    # )
    result = generate {
        position: { row: 1, column: 1 },
        element: {
            types: [],
            funcs: [],
            mems: [],
            datas: [
                {
                    position: { row: 2, column: 4 },
                    element: {
                        offset: [
                            I32Const {
                                position: { row: 1, column: 1 },
                                element: 8,
                            },
                        ],
                        bytes: "hello world",
                    },
                },
            ],
            imports: [],
            exports: [],
        },
    }

    result
    == [
        0x00,
        0x61,
        0x73,
        0x6d,
        0x01,
        0x00,
        0x00,
        0x00,
        0x0b,
        0x11,
        0x01,
        0x00,
        0x41,
        0x08,
        0x0b,
        0x0b,
        0x68,
        0x65,
        0x6c,
        0x6c,
        0x6f,
        0x20,
        0x77,
        0x6f,
        0x72,
        0x6c,
        0x64,
    ]

expect
    result = generateU32 624485
    result == [0xE5, 0x8E, 0x26]

expect
    result = generateU32 3
    result == [0x03]

expect
    result = generateU32 0
    result == [0x00]

expect
    result = generateU32 123456
    result == [0xc0, 0xc4, 0x7]

expect
    result = generateVector [624485, 3, 0, 123456] generateU32
    result == [0x04, 0xE5, 0x8E, 0x26, 0x03, 0x00, 0xc0, 0xc4, 0x7]

expect
    result = generateMem { min: 1 }
    result == [0x00, 0x01]

expect
    result = generateData {
        offset: [
            I32Const {
                position: { row: 1, column: 1 },
                element: 8,
            },
        ],
        bytes: "hello world",
    }

    result == [0x00, 0x41, 0x08, 0x0b, 0x0b, 'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd']

expect
    result = generateInstruction
        (
            I32Const {
                position: { row: 1, column: 1 },
                element: 8,
            }
        )

    result == [0x41, 0x08]

expect
    result = generateInstruction (I32Store { row: 1, column: 1 })
    result == [0x36, 0x02, 0x00]

expect
    result = generateType I32
    result == [0x7f]

expect
    result = generateFuncType {
        identifier: "helloFunc",
        param: [
            { element: I32, position: { row: 1, column: 1 } },
            { element: I32, position: { row: 1, column: 1 } },
        ],
        result: [{ element: I32, position: { row: 1, column: 1 } }],
    }

    result == [0x60, 0x02, 0x7f, 0x7f, 0x01, 0x7f]
