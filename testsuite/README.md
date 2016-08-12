# Testsuite

## How to update the testsuite :

If there is new plugins or linters, `testsuite/.ocplint` needs to be updated.
It needs to have all plugins/linters activated.

Once this is done, just run the script in `plugins/` :

`sh make_db_test.sh`