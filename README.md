# wordbots-parser
[![CircleCI](https://circleci.com/gh/wordbots/wordbots-parser.svg?style=svg)](https://circleci.com/gh/wordbots/wordbots-parser)

[![](http://imgur.com/q7lBCUn.png)](https://www.patreon.com/wordbots)

A game-semantics CCG parser for a card game that doesn't exist yet.

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
