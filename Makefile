elm = build/elm.js
js = build/index.js build/index.js.map

all: $(elm) $(js)

clean:
	rm -rf build

$(js): $(elm) mbv.js webpack.config.js
	mkdir -p build
	node_modules/.bin/webpack

$(elm): Main.elm 
	mkdir -p build
	elm make Main.elm --output build/elm.js
