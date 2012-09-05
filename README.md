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

#### When installing OpenLDAP from sources:

It might be necessary to make some modifications to default `etc/slapd.conf`.
First of all, you need 4 schemas and they have to be placed in config
in right order.

    include     [PREFIX]/etc/openldap/schema/core.schema
    include     [PREFIX]/etc/openldap/schema/cosine.schema
    include     [PREFIX]/etc/openldap/schema/inetorgperson.schema
    include     [PREFIX]/etc/openldap/schema/nis.schema

`[PREFIX]` should be replaced with OpenLDAP installation directory.

You will also need to set domain name. Navigate to the bottom
of the file and set following lines:

    suffix      "dc=example,dc=com"
    rootdn      "cn=admin,dc=example,dc=com"

Next, use `slappasswd` and paste its whole output (including `{SSHA}`)
after `rootpw`:

    rootpw  {SSHA}SomeHashHere

It might be necessary to `ldapadd` another file before `priv/ejabberd.ldif`.
If `ldapadd priv/ejabberd.ldif` fails, create new file named `initial.ldif`
with following contents:

    dn: dc=example,dc=com
    objectclass: dcObject
    objectclass: organization
    o: Example Company
    dc: example

And run:

    sudo ldapadd -x -D cn=admin,dc=example,dc=com -W -f initial.ldiff


Running test dependencies elsewhere
-----------------------------------

The tests connect to ejabberd and the depended-on servers on localhost.

To avoid installing everything on your development machine, you could forward
ports to some other machine where you can run a server. For example, for MySQL,
run

    ssh -L 3306:localhost:3306 mysql.server.hostname