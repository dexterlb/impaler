module PartialValues
    ( PValue
    , PValueItem
    )

where


data PValue v m
    = PValue DebugInfo (PValueItem v m)
    | PConst PValue     -- ^ marker to stop partial evaluation

data PValueItem v m
    -- | A symbol is a basic atom
    | Symbol            Identifier
    -- at some point symbols need to be interned, but for now Text will do

    -- | S-expression building blocks
    | Pair              (PValue v m) (PValue v m)
    | Null

    -- | The ultimate sin: special forms are first-class citizens
    | SpecialForm       SpecialForm

    -- | A callable object that may optionally have side effects and/or
    -- | Look at the environment (the latter should really not be abused)
    | Func              (Env v m -> PCallback v m -> PValue v m -> m ())

    -- | Since we don't have things like panics or exceptions,
    -- | we encapsulate failure as a separate value type, which makes
    -- | handling errors easier
    | Fail              (PValue v m)

    -- | Encapsulate user-defined data structures (the user being
    -- | the one who embeds the language). Things like lazy lists,
    -- | hash maps, file handles, etc, will all go here
    | ExternalVal       v

    -- | Some data types that can be parsed directly as part of the
    -- | S-expressions
    | Str               Text
    | Num               Float
    | Bool              Bool
