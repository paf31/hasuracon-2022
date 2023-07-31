## Code or No Code - Extending Hasura with Scripting Capabilities

These are the incomplete notes I prepared for a talk at HasuraCon 2022, for a talk which I was unable to actually give due to other commitments. It consists of an extended demo of [Dovetail](https://github.com/paf31/dovetail/) along with a discussion of some of the trade-offs between code and no-code architectures. The talk is incomplete but the demo is a fairly comprehensive tour of the capabilities of Dovetail. 

---

## The need for a scripting language for Haskell

Many programming languages have adequate solutions to the scripting problem - for many programming tasks, you don't necessarily want to write code in the host language for all tasks.

For example, Lua makes an excellent scripting language for C and other languages, and it can be easily embedded. Instead of implementing something like SQL, Redis embeds Lua and allows the developer to run Lua scripts which have access to the low-level operations of Redis. It runs those scripts with transactional semantics, making Lua an excellent replacement for something like SQL, for the sorts of problems where you would use Redis in place of a relational database.

Lua is also widely used for game scripts: the core logic of a game engine, which must be fast, can be written in a low-level language, and the game engine can delegate to scripts written in Lua for less performance-critical code. Those scripts can delegate back to low-level procedures, and so on.

In interpreted languages, the host language itself can act as a lowest-common-denominator scripting language, and there are plenty of examples of that sort of approach in JavaScript, Python, and so on.

I recently began to think about what such a scripting language would look like for Haskell. Haskell certainly would benefit from the ability to move compilation and execution of certain inessential code out of Haskell itself - the Haskell toolchain is excellent, but certainly scales poorly as the complexity of projects increases - if we could move inessential code into scripts, then we would be able to spend our compilation time budget on the parts that would really benefit from Haskell's strong typing and optimization.

There are some candidates for a Haskell scripting language, but no clear winner. Arguably, a scripting language for Haskell should take advantage of Haskell's strengths, including strong types, and integrate closely with Haskell. PureScript seemed like a natural candidate, and my deep experience with the core building blocks of PureScript allowed me to see what an implementation would look like. 

So, last year, I started working on the [Dovetail PureScript interpreter](https://hackage.haskell.org/package/dovetail).

## A quick tour of Dovetail

The best way to get a sense of how Dovetail works is to look at some examples. Here is the Hello World of Dovetail programs:

```haskell
example = runInterpret @Text do
  ffi prelude
  _ <- build "module Main where main = \"example\""
  liftEval $ evalMain (ModuleName "Main")
```

This program runs the Dovetail interpreter with [`runInterpret`](https://hackage.haskell.org/package/dovetail-0.1.0.0/docs/Dovetail.html#v:runInterpret). Inside the `do` block, it first imports a basic standard library using the [`ffi`](https://hackage.haskell.org/package/dovetail-0.1.0.0/docs/Dovetail.html#v:ffi) function, then builds a PureScript module from source code, before evaluating `main` in that module. The type application `@(Eval Text)` disambiguates the return type of the computation, and the type of `example` itself ends up being inferred as

```haskell
example :: Either (InterpretError Identity) Text
```

Evaluating this in the REPL should give the result `Right "example"`.

That's the basic idea behind Dovetail: you can import FFI modules which are implemented in Haskell (but higher-order functions allow for bidirectional calls), build modules and evaluate values. But there is a lot built on top: a full implementation of the PureScript standard library, a rudimentary REPL and debugger, support for evaluating PureScript's "corefn" intermediate representation, a small DSL for building FFI libraries, and support for creating PureScript types at runtime from Haskell values.

If you are interested, the [GitHub repo](https://github.com/paf31/dovetail/) contains some examples which should be quite approachable.

## Hasura and Scripting

[Hasura](http://hasura.io) is a well known Haskell open source project - it is best known for being able to turn your Postgres database into a GraphQL server, but it can do a lot more. Hasura can also bring in data from other sources: other relational databases, REST services, and other existing GraphQL sources; it can stitch these together into a single GraphQL schema, joining data in an efficient way; it can respond to change events from those data sources, and invoke webhooks when changes happen; it can batch and cache requests for efficiency, and a lot more. It is a central point of management for many hosted data services.

And it does all of this without the user having to write a line of code - it is a _No Code_ solution, which is configured in YAML files, and extended using webhooks and REST services if necessary. Many decisions are made automatically, with sensible overridable defaults, on behalf of the user.

So naturally, having worked on Dovetail, which aimed to make it possible to script complex Haskell programs, and also on Hasura, a complex Haskell program, I wondered if the two could be combined. It was in many ways a natural fit - Hasura was No Code, but what if it could be scriptable as well? The user could have the benefit of Code and No Code, side-by-side. No Code for the vast majority of cases where the defaults were perfect, and Code for the difficult corner cases, where the user might otherwise have had to bring together several services and integrations in order to build something custom.

In trying to answer the question "what would Hasura look like if it had embraced scripting instead of, or in addition to, No Code?", I created a simple prototype of Hasura-with-scripting. But before I talk about that, I need to talk about a new feature of Hasura.

## Hasura data connectors

On paper, it was a simple matter of adding a dependency on Dovetail to the open source Haskell project, and adding FFI implementations for the key Hasura features, and then running user programs at the appropriate points in the code. However, PureScript and Hasura both pull in a lot of Haskell dependencies, and resolving those turned out to be intractable in a reasonable amount of time, so I've scrapped that idea for now.

However, at HasuraCon itself, Hasura announced its new [data connector](https://hasura.io/blog/hasura-graphql-data-connectors/) feature. You should read the blog post, but the idea is to allow users to extend Hasura with support for new database-like data sources by writing a web service. Unlike Hasura's "actions" which wrap existing REST services, these services are designed around integration with a whole data source. Hasura will query the capabilities of the service, and then send serialized ASTs which should be interpreted.

I saw this as an opportunity to build the prototype I wanted - Hasura can now make use of an interpreter, and Dovetail itself _is_ an interpreter, so there should be a natural way to combine them.

## Building data connectors with Dovetail

I called my new prototype `graphql-supercharger` (because naturally, it supercharges your `graphql-engine` with scripting capabilities).

Now, to think about useful and possible ways to make use of Dovetail in a data connector, we need to understand what a data connector needs to be able to do. To get a sense of the query AST which will be sent to a data connector, we can look at the data definitions in the [Haskell source code](https://github.com/hasura/graphql-engine/blob/edc29ce0d46cd54005d90d4b3ff0252c93f96222/server/src-dc-api/Hasura/Backends/DataConnector/API/V0/Query.hs#L74-L84). A query contains the data of an idealized GraphQL query: a selection of fields to fetch, pagination information, a predicate, and an expression by which rows should be sorted.

One obvious way in which to use Dovetail is to let the user express the AST interpreter itself, but that is probably not going to be very useful as a tool - why would a user choose to write the whole service in an embedded DSL when it could be written much more easily in a general purpose language? A DSL should make this job easier, allowing the user to express only the essential details of the service, and performing any boilerplate code generation on the user's behalf.

A second way in which we could try to use Dovetail is to let the user express a data source as a PureScript _value_, and to try to generate all of the pagination, filtering and sorting code from that high-level specification. But if you think about that, it becomes obvious that such a design would have to trade off either completeness or performance - it's not going to be possible to be able to push down every predicate, every sort, and every pagination request, at least not without severely restricting the set of data sources it would be able to express.

So, let's instead take some inspiration from the use of Lua as a scripting language for Redis. Redis doesn't try to use Lua to express _new_ data sources, but instead to let the user build queries across data sources which have already been defined using a set of efficient primitive operations.

Instead of trying to script the creation of a data source, we can try to script the creation of a _data source transformation_. We need to be able to push down filters and order-by expressions, but _Hasura can already do that_ for all of its own database-defined resolvers, so let's leverage that capability.

`graphql-supercharger` lets the user define a new data source from any existing Hasura query root field which is defined by a database table or view, modifying it at the time of definition in a few useful ways:

- adding an additional predicate, which will be pushed down, via Hasura, to the database
- fetching additional fields, either from other Hasura sources, or from external HTTP services

## Configuring and running the server

`graphql-supercharger` defines two config files: a `config.yaml` file which is supposed to change rarely, and a `server.purs` file, which defines the interesting logic of the server in the PureScript DSL, and which is supposed to be able to change frequently, with hot reloading.

The user can create an initial `config.yaml` file from a running Hasura instance with `graphqlsupercharger init`. For a simple albums/artists/tracks schema, it will generate something like the following in `config.yaml`:

```yaml
source: server.purs
engineUrl: http://localhost:9000
tables:
  albums:
    columns:
      - name: artist_id
        type: number
      - name: id
        type: number
      - name: title
        type: string
  artists:
    columns:
      - name: id
        type: number
      - name: name
        type: string
        nullable: true
  tracks:
    columns:
      - name: album_id
        type: number
        nullable: true
      - ...
```

The idea is that this file will keep a copy of the schema information as a source of reference, so that typechecking and reloading of the PureScript sources can always be done quickly against a stable source of truth.

The same command will generate a `server.purs` file which looks like this:

```purescript
module Main where

import Supercharger

config = defaults
 { albums 
   { predicate = \{ artist_id, id, title } ->
       ?predicate
   , extras = \{ artist_id, id, title } ->
       ?extras
   } 
 }
```

Note that the `init` command only generates a configuration for the first table, `albums`, and the initial configuration has two _typed holes_ which need to be filled in before the server can be run. The first, `?predicate`, is a placeholder for the _additional predicate_ that should be pushed down to Hasura when the query is run. The second, `?extras`, is a placeholder for a record of additional fields to be returned with each row. These additional fields do not support filtering or sorting since those cannot be pushed down to Hasura, since they are defined entirely separately.

As a basic example of a working server, we can fill in the first typed holes with a simple predicate, and the second with a static response for a single extra field:

```purescript
module Main where

import Supercharger
import Prelude ((<>))

config = defaults
 { albums 
   { predicate = \{ artist_id } ->
       artist_id == 42.0
   , extras = \_ ->
       { greeting: "Hello, World!" }
   } 
 }
```

The predicate here illustrates the most obvious use case: extending an existing Hasura data source with _custom authorization rules_ defined in the scripting language.

## Making types work for us

We can already see one benefit of using a typed language like PureScript for the DSL: types are a tool, but they should be a _generative tool_. That is, put to good use in a DSL, they should actually save the user some work. Not only do we use types to check the _validity_ of our code (like we are using the types of table columns to give types to our fields like `artist_id` which are brought into scope in the predicate body, which are then checked at the point of use), but we also _use types_ to generate additional information, like the schema of the `extras` fields. The user never explicitly said that there is a single `greeting` field of type string, but we could infer that from the PureScript types.
