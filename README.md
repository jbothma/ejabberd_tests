Test suite for ESL's branch of ejabberd
=======================================

This repository contains test code for ejabberd.
It's used mainly for [ESL's branch of ejabberd](https://github.com/esl/ejabberd),
but one that may be reused in other projects.

Test dependency setup
---------------------

### MySQL

The MySQL database schema is in ejabberd/apps/ejabberd/priv/mysql.sql.

The variable table_type has been removed in MySQL 5.2 and should be replaced
with storage_engine in this version and later.

### Redis

For testing, running redis-server 2.4.16 in the directory it was compiled
seems to be sufficient.

### LDAP

TODO: add ldif file for test configuration and instructions for setting it up
for test.

Running test dependencies elsewhere
-----------------------------------

The tests connect to ejabberd and the depended-on servers on localhost.

To avoid installing everything on your development machine, you could forward
ports to some other machine where you can run a server. For example, for MySQL,
run

  ssh -L 3306:localhost:3306 mysql.server.hostname