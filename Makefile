
install:
	npm install

dev:
	npm run dev

build: install
	rm dist/*
	npm run build

test: install
	npm run test
