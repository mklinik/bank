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

After you have a role, create the databases for test and production

```
$ createdb bank
$ createdb bank-test
```

To run the server:

```
$ lein ring server
```

To run the tests:

```
$ lein test
```

To run only tests in one namespace

```
$ lein test bank.database-test
```
