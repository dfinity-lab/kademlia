# Hypotheses

- ~~UDP packets are being dropped~~
  - The test goes over localhost, seems very unlikely
- ~~One or more nodes is being banned~~
  - Test failure still occurs with all ban-related code removed
- ~~Something to do with the `STORE` protocol~~
  - Test failure still occurs with all `STORE`-related code removed
- ~~Something to do with `Network.Kademlia.HashNodeId`~~
  - Test failure still occurs with this module removed
- ~~Interference from another test~~
  - Test failure still occurs when the test executable is launched with a `-p` filter
- ~~Non-determinism~~
  - Test failure reliably occurs, even if you don't fix a QuickCheck seed
- ~~A bug in the parsing/serialization code~~
  - Seems unlikely since there are no parsing errors reported in the log output
- A bug in the `Tree` module
- A bug in the `ReplyQueue` module
- The threshold is just wrong
