# Overview

Some use-cases exist, where many features of the Hydra platform are useful, but interfacing with the entire Hydra node, especially with respect to IO, is impractical. For a use-case which calls for different persistence requirements from the Hydra default, it initially might seem there are two options:

1. One might decide to fork the Hydra code-base, to make any customizations desired. This comes at the cost of maintinance burden, as internal code is inherently more unstable and less documented than external-facing interfaces.

2. The alternative would be to instead make do with running a full Hydra node, and preparing a persistence file before the Hydra runs, or parsing the file as it is written. This comes at the cost of control, code duplication, resource usage, and still relies on interfacing with an unstable external API (the persistence file on-disk).

To better enable these use-cases, Hydra supports the concept of alternate Event Sinks and a single alternate Event Source. These respectively represent a way to seed the initial transactions a Hydra node loads upon startup (a generalization of restoring from the persistence file), and ways to persist new transactions processed at run-time (a generalization of appending to the persistence file).

Multiple Event Sinks may be used simultaneously, but currently only one Event Source may be used at a time. The Event Source is only loaded upon startup. Each Event Sink is run upon each new transaction. Currently, the order must be specified by customizing the order of the Event Sink list in the Hydra node source code, in the eventSinks parameter to hydrate, invoked in Hydra.Node.Run.run [here](https://github.com/SundaeSwap-finance/hydra/blob/4785bd86a03b92ba8fa8fb34c9d485a1e2f4f7d7/hydra-node/src/Hydra/Node/Run.hs#L104)

The default Hydra file-based persistence, is implemented as an Event Sink and Source pair. They do not need to be used in tandem; it is possible to use the default Event Source which processes previous transactions from a file on disk, combined with an Event Sink which could, for example, store new transactions on S3, on several machines, or not at all.

Currently, there is no CLI API to toggle which sources and sinks are utilized, this must be done by the implementor of the sources and sinks. See the source and sink example, [here](https://github.com/SundaeSwap-finance/hydra/blob/4785bd86a03b92ba8fa8fb34c9d485a1e2f4f7d7/hydra-node/src/Hydra/Node/Run.hs#L97), where the Event Sinks and Source are toggled by added CLI Options.

To see an example of S3 and AWS Kinesis Event Sources and Sinks, see the doug_hydra_changes branch [here](github.com/SundaeSwap-finance/hydra). In particular, see TODO: new link once the branch is in a more publishable state. Also, I'm not sure if we have this branch public yet due to Gummiworm code living in the same code.

Hydra also supports an offline mode, which allows for disabling the Layer 1 interface (that is, the underlying Cardano blockchain which Hydra heads use to seed funds and ultimately funds are withdrawn to). This offline mode makes operation only influenced by the HTTP/WebSocket APIs, and the configured Event Sinks and sources. While separate from the Event Sinks and Event Source functionality, disabling Layer 1 interactions allows for further customization, enabling use-cases which would otherwise require running and configuring an entire Layer 1 private devnet. TODO: link more offline mode tutorials.

