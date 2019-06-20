# Archetype IDL

**WORK IN PROGRESS**

**TODO: logo**
```
arch:Type
```

Archetype is another attempt at IDL (interface description/definition language)
inspired by [Dhall](https://dhall-lang.org/) configuration language.


## Comparison with Protocol Buffers

**TODO**


## Comparison with Apache Thrift

**TODO**


## Main Ideas

*   Protocol/interface is described using types.  RPC/REST calls are just types
    too.

*   Has a notion of primitive types that code generator has to understand.  It
    allows us to define:

    ```
    prim type gRPC : forall(request : Type) -> forall(response : Type) -> gRPC request response
    ```

    Same using Unicode syntax:

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


# Example

```
-- Primitive types are those that are already known to the code generator.
-- It's just a way how to explicitly surface them in Archetype.

prim type NonEmpty : forall(a : Type) -> NonEmpty a
prim type UUID : Type

-- REST and RPC calls are modeled as primitive types as well.  This allows us
-- to support any transport layer supported by individual code generator.
prim type RpcCall :
    forall(request : Type)
  -> forall(response : Type)
  -> RpcCall request response

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

prim type Response :
    forall(error : Type)
  -> forall(r : Type)
  -> Response error r

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

## Compilation Stages

```
     ┌───────────┐
     │ Archetype │
     │  source   │
     │   code    │
     └───────────┘
           │
    Parse  │
           ▼
     ┌───────────┐
     │ Archetype │
     │           │
     │    AST    │
     └───────────┘
           │
    Embed  │   ┌───────┐
           ▼   │       │
     ┌───────────┐     │  Type check and normalise
     │   Dhall   │     │
     │           │<────┘
     │    AST    │
     └───────────┘
           │
 Simplify  │
           ▼
     ┌───────────┐
     │           │
     │    IR     │
     │           │
     └───────────┘
           │
Serialise  │
           ▼
     ┌───────────┐
     │           │
     │   Dhall   │
     │           │
     └───────────┘
           │
     Send  │
           ▼
     ┌───────────┐
     │   Code    │
     │           │
     │ Generator │
     └───────────┘
```
