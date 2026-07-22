.PHONY: uber run

build:
	clojure -T:build uber

run:
	java -jar companion.jar
