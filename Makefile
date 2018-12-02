
install:
	npm install

dev:
	npm run dev

build: install
	rm -f dist/*
	npm run build

test: install
	npm run test
