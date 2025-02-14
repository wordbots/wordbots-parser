# wordbots-parser

A game-semantics CCG parser for [Wordbots](http://wordbots.io). (See also the [`wordbots-core` repo](https://github.com/wordbots/wordbots-core) that contains the core game code.)

## Running as console
```
sbt console
> parse("text to parse")
```

## Running as server
```
sbt run
```
Now you can send requests to `http://localhost:8080/parse?input=<TEXT>&format=<js or svg>`.

To connect to this server from your local [`wordbots-core`](https://github.com/wordbots/wordbots-core) instance, set `const USE_LOCAL_PARSER = true` in [`constants.ts`](https://github.com/wordbots/wordbots-core/blob/master/src/common/constants.ts) (and make sure that `LOCAL_PARSER_PORT` is correct), then start `wordbots-core` (`yarn && yarn start`).

## Testing
```
sbt ";scalastyle;test"
```
