Test suite for ESL's branch of ejabberd
=======================================

This repository contains test code for ejabberd.
It's used mainly for [ESL's branch of ejabberd](https://github.com/esl/ejabberd),
but one that may be reused in other projects.

Test dependency setup
---------------------

### MySQL

The MySQL database schema is in ejabberd/apps/ejabberd/priv/mysql.sql.

The `table_type` variable has been removed in MySQL 5.2 and should be replaced
with `storage_engine` in this version and later.

### Redis

For testing, running redis-server 2.4.16 in the directory it was compiled
seems to be sufficient.

### LDAP

Install an LDAP server. This example will use OpenLDAP on Ubuntu 12.04

Make example.com resolve to your host from the ldap server, for example by
adding the following to /etc/hosts

    127.0.0.1 example.com

Install the server and utilities

    sudo apt-get install slapd ldap-utils

Configure LDAP to authenticate the test user

    sudo ldapadd -x -D cn=admin,dc=example,dc=com -W -f priv/ejabberd.ldif

Now you can do a quick test

    $ erl
    Erlang R15B01 ...
    
    Eshell V5.9.1  (abort with ^G)
    1> {ok, Handle} = eldap:open(["192.168.56.102"]).
    {ok,<0.33.0>}
    2> eldap:simple_bind(Handle, "uid=john,ou=People,dc=example,dc=com","johnldap").
    ok
    3> eldap:simple_bind(Handle, "uid=john,ou=People,dc=example,dc=com","johnlda"). 
    {error,invalidCredentials}

Running test dependencies elsewhere
-----------------------------------

The tests connect to ejabberd and the depended-on servers on localhost.

To avoid installing everything on your development machine, you could forward
ports to some other machine where you can run a server. For example, for MySQL,
run

    ssh -L 3306:localhost:3306 mysql.server.hostname