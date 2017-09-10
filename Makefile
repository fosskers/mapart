STACK ?= /bin/stack
PORT ?= 8081

elm:
	cd ui/ && elm-make Art.elm --output art.js && mv *.js ../assets/

build: elm
	cd server/ && $(STACK) build

# Needs to run with RTS ops
start:
	cd server/ && $(STACK) exec -- server  #--port $(PORT)

go: build start
