elm = build/elm.js

all: $(elm) 

clean:
	rm -rf build

watch:
	while true; do \
		make all && elm-test; \
		inotifywait -qre close_write .; \
	done

$(elm): src/Vector.elm src/Tile.elm src/Proto.elm
	mkdir -p build
	elm make src/Vector.elm --output build/elm.js
