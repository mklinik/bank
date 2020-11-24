# this file is supposed to be sourced

request-retrieve-account() {
  curl -v 'http://localhost:3000/account/42'
  echo ""
}

request-account() {
  curl -v 'http://localhost:3000/account' \
    -d '{ "name": "Mr. Black" }' \
    -H "Content-Type: application/json"
  echo ""
}

request-echo() {
  curl -v 'http://localhost:3000/echo' \
    -d '{ "name": "Mr. Black" }' \
    -H "Content-Type: application/json"
  echo ""
}
