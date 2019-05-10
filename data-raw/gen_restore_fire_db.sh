#!/bin/bash

set -e
set -u

# Get arguments
for ARGUMENT in "$@"
do

    KEY=$(echo $ARGUMENT | cut -f1 -d=)
    VALUE=$(echo $ARGUMENT | cut -f2 -d=)   

    case "$KEY" in
	db)	db=${VALUE} ;;
	dump)   dump=${VALUE} ;;     
	*)   
    esac    

done

echo "db = $db"
echo "dump = $dump"


RUN_ON_MYDB="psql -X -U alain --set ON_ERROR_STOP=on --set AUTOCOMMIT=off $db"
PSQL="psql -U alain -d $db -x -c "

# Create the SQL database:  
# https://stackoverflow.com/a/30642050
if psql -lqt | cut -d \| -f 1 | grep -qw $db; then
    echo "$db already exists"
    dropdb $db
    createdb $db
else
    createdb $db 
    echo "$db database has been created"
fi
echo "List of the existing database(s): `psql -l`"


# Import the SQL scheme in the database:
pg_restore -d $db $dump
