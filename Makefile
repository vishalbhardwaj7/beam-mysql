# Because mysqld is special, SIGINT (Ctrl-c) won't work, so send this SIGQUIT
# (Ctrl-\). - Koz
start: /tmp/mysql
	mysqld --user=${LOGNAME} --datadir=$< --socket=/tmp/mysql/mysqld.sock \
		--log-error-verbosity=1 \
		--character-set-server=latin1 \
		--collation-server=latin1_swedish_ci \
		--init-file=${PWD}/init.sql

/tmp/mysql: 
	mysqld --initialize-insecure --user=${LOGNAME} \
		--datadir=$@ --explicit-defaults-for-timestamp \
		--log-error-verbosity=1 \
		--character-set-server=latin1 \
		--collation-server=latin1_swedish_ci
