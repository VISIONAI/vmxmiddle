docker run --rm --link db_middle:db -i -t gdoteof/d-postgres /bin/bash -c "PGPASSWORD=docker psql -U docker -h db docker"
