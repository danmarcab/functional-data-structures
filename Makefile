
install:
	npm install

dev:
	npm run dev

build: install
	rm -f dist/*
	npm run build
	cp public/favicon.ico dist/favicon.ico

test: install
	npm run test
