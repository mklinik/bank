# this file is supposed to be sourced

request-retrieve-account() {
  curl -v 'http://localhost:3000/account/42'
  echo ""
}

request-create-account() {
  curl -v 'http://localhost:3000/account' \
    -d '{ "name": "Mr. Black" }' \
    -H "Content-Type: application/json"
  echo ""
}

request-deposit() {
  curl -v 'http://localhost:3000/account/42/deposit' \
    -d '{ "amount": 100 }' \
    -H "Content-Type: application/json"
  echo ""
}

request-echo() {
  curl -v 'http://localhost:3000/echo' \
    -d '{ "name": "Mr. Black" }' \
    -H "Content-Type: application/json"
  echo ""
}
