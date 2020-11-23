#!/usr/bin/env bash

request() {
  curl -s http://localhost:3000/test -d '{"operation": "+", "operands": [23, 42] }'
}

for i in {0..999}
do
  # running requests in a subshell suppresses job control output
  (request >/dev/null &)
done
