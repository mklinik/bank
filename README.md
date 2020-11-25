# bank

A simlpe web API in Clojure

# Development

Before starting running the server or the tests, make sure that postgres is
running and that you have a working role.

```
$ su -
# su - postgres
$ psql
postgres=# CREATE ROLE mkl LOGIN;
postgres=# ALTER ROLE mkl WITH CREATEDB;
postgres=# ALTER ROLE mkl WITH password 'w00t';
```

To run the server:

```
$ lein ring server
```

To run the tests:

```
$ lein test
```
