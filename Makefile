build-docker-image:
	- cp $(stack path --local-install-root)/bin/sql2er-exe .
	- docker build --no-cache . -t tusharknight8/sql2er
	- rm ./sql2er-exe