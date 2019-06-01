# Archetype IDL

**WORK IN PROGRESS**

Archetype is another attempt at IDL (interface description/definition language)
inspired by [Dhall](https://dhall-lang.org/) configuration language.

Main ideas:

*   Protocol/interface is described using types.  RPC/REST calls are just types
    too.

*   Has a notion of primitive types that code generator has to understand.  It
    allows us to define:

    ```
    prim type gRPC : ∀(request : Type) → ∀(response : Type) → gRPC request response
    ```

    If the code generator doesn't understand gRPC then it will fail.

*   Syntax is an extension of [Dhall](https://dhall-lang.org/) language.  Dhall
    is also used to encode Archetype intermediate representation consumed by
    code generator.

*   Code generators are explicit; there is no default code generator.  This is
    to prevent implementation to be tied to a specific representation too much,
    and it also encourages users to think of code generators as something they
    can provide them selves.

    As a consequence service API is defined by interface description in the
    form of Archetype IDL file, and suitable code generator(s).


# Logo

TODO:
```
arch:Type
```


# Example

```
-- Primitive types are those that are already known to the code generator.
-- It's just a way how to explicitly surface them in Archetype.

prim type NonEmpty : ∀(a : Type) → NonEmpty a
prim type UUID : Type

-- REST and RPC calls are modeled as primitive types as well.  This allows us
-- to support any transport layer supported by individual code generator.
prim type RpcCall
  : ∀(request : Type)
  → ∀(response : Type)
  → RpcCall request response

-- Normal types are those for which code will be generated.

type UserId = UUID

-- Enums are supported as a special case of sum types.
type UserRole = < Admin | NormalUser >

-- A user, what a surprise.
type User =
  { id : UserId
  , full-name : Text
  , emails : NonEmpty Text
  , verified : Bool
  , role : UserRole
  }

type GetUserError =
  < NoSuchUser
  | AccountDisabled
  | OtherError : Text
  >

-- We can define polymorphic types.  Code generators may need to make them
-- monomorphic when generating code for languages that do not support them.
-- That can be achieved by requiring top-level type (usually RPC or REST call)
-- to be present.  For those the polymorphic types need to be fully applied,
-- therefore, code generator can define something like `GetUserResponse`
-- instead.
type Response =
    λ(error : Type)
  → λ(r : Type)
  → < Error : error
    | Response : r
    >

type rpc-get-user = RpcCall UserId (Response GetUserError User)
```


# Usage

```
ark codegen --backend=EXECUTABLE [--backend-argument=ARGUMENT ...] [FILE]
ark lint [FILE]
ark diff FILE FILE
ark help
ark version
ark {--help|-h}
ark {--version|-V}
```


# Architecture

In principle Archetype compiler `ark` does this:

```
                                          ┌────────────────────┐
                ┌─────────┐              ┌────────────────────┐│
 Interface      │         │    Dhall     │                    ││        Native
description ───>│   ark   │─────────────>│   code generator   ││─────>   code
(Archetype)     │         │   via pipe   │                    │┘   (.hs, .psc, etc.)
                └─────────┘              └────────────────────┘
```

However, the implementation of code generator should behave as a pure function,
or at least as much as one can be approximated.  This allows us to safely
examine the output of code generator.

```
       Archetype
           │
           │
┌──────────│──────────┐
│ ark      ▼          │
│   ┌─────────────┐   │
│   │             │   │
│   │    front    │   │
│   │     end     │   │
│   │             │   │
│   └─────────────┘   │
│          │          │
│      IR  │          │
│          ▼          │
│   ┌─────────────┐   │   IR (Dhall)   ┌─────────────┐
│   │             │───────────────────>│             │
│   │  back end   │   │                │    code     │
│   │  executor   │   │  Files (Dhall) │  generator  │
│   │             │<───────────────────│             │
│   └─────────────┘   │                └─────────────┘
│          │          │
└──────────│──────────┘
           │
           ▼
       Generated
         Files
```
