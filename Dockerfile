FROM mysql:5
ENV MYSQL_ALLOW_EMPTY_PASSWORD=yes
ENV MYSQL_DATABASE=test
COPY init.sql /docker-entrypoint-initdb.d/
