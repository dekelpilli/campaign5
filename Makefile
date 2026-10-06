.PHONY: uber run

build:
	clojure -T:build uber

run:
	@(until nc -z 127.0.0.1 5555 2>/dev/null; do kill -0 $$$$ 2>/dev/null || exit 1; sleep 0.5; done; \
	  open -na "Brave Browser" --args --profile-directory=Default http://127.0.0.1:5555/) & \
	exec java -jar companion.jar
