# this file is supposed to be sourced

request-retrieve-account() {
  curl -v "http://localhost:3000/account/$1"
  echo ""
}

request-audit-log() {
  curl -v "http://localhost:3000/account/$1/audit"
  echo ""
}

request-create-account() {
  curl -v 'http://localhost:3000/account' \
    -d "{ \"name\": \"$1\" }" \
    -H "Content-Type: application/json"
  echo ""
}

request-deposit() {
  curl -v "http://localhost:3000/account/$1/deposit" \
    -d "{ \"amount\": $2 }" \
    -H "Content-Type: application/json"
  echo ""
}

request-withdraw() {
  curl -v "http://localhost:3000/account/$1/withdraw" \
    -d "{ \"amount\": $2 }" \
    -H "Content-Type: application/json"
  echo ""
}

request-echo() {
  curl -v 'http://localhost:3000/echo' \
    -d '{ "name": "Mr. Black" }' \
    -H "Content-Type: application/json"
  echo ""
}
