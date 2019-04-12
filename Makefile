.PHONY: test
test:
	stack test --ghc-options="-DINSPECTION_TESTING=1"
