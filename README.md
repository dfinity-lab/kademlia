kademlia
========

kademlia is a haskell libraray implementing the [Kademlia][wiki_kademlia]
[distributed hash table][wiki_dht].

It aims to be flexible and handle all the low-level work, in order to allow
for easy integration of the DHT-technology into applications.

The library is built after the original [Kademlia Paper][paper_kademlia].

For more information, take a look at the libraries [hackage page][hackage].

[wiki_kademlia]: https://en.wikipedia.org/wiki/Kademlia
[wiki_dht]: https://en.wikipedia.org/wiki/Distributed_hash_table
[paper_kademlia]: http://pdos.csail.mit.edu/~petar/papers/maymounkov-kademlia-lncs.pdf
[hackage]: https://hackage.haskell.org/package/kademlia

## Implementation note (to understand some parts of code)

### Flow to find nodes by key:

`lookupNode` => `runLookup go inst nid`: create chan `replyChan`, launch `go`

`go`:

1. send signal
2. `expect` registration `(reg, replyChan, timeoutTid)` for this `(signal, nodeId)`
3. wait for reply on `reg`, continue if needed
   (we handle nodes we know now and check whether we want to query more neighbors)

To wait for reply on `(reg, replyChan, timeoutTid)` we listen to `replyChan`

### How incoming requests are dispatched, workers:

1. `DFINITY.Discovery.Networking.startRcvProcess`:
   ```
   read `(signal, nodeId)` from UDP socket, combine it to reply `reply` (pure operation)
   write `reply` to `timeoutChan`
   ```
2. `DFINITY.Discovery.Instance.receivingProcess`:
   ```
   read `reply` from `timeoutChan`
   (`notResponse` || `expectedReply`):
       check either:
         * there was a registration `reg` for our `reply`
         * `reply` is a request (not response)
       if call `receivingProcessDo` which calls `dispatch`
   ```

   `DFINITY.Discovery.ReplyQueue.dispatch`:
   ```
   * we check whether there is registration corresponding to `reply`
     + if there is registrartion `(reg, replyChan, timeoutTid)` then we
           kill timeout thread by it's id `timeoutTid`
           write `reply` to `replyChan`
           // further it would be handled by thread which sent `signal` and was waiting for reply
     + if there is no registration // note, that this may happen only for requests
           write `reply` to `defaultChan`
   ```
3. `DFINITY.Discovery.Instance.backgroundProcess`:
   ```
   read `reply` from `defaultChan` //note that only requests can get here
   `handleCommand` for `reply`
   ```
