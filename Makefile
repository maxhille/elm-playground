elm = build/elm.js
js = build/index.js build/index.js.map

all: $(elm) $(js)

clean:
	rm -rf build

$(js): $(elm) webpack.config.js
	mkdir -p build
	node_modules/.bin/webpack

$(elm): src/Vector.elm 
	mkdir -p build
	elm make src/Vector.elm --output build/elm.js
