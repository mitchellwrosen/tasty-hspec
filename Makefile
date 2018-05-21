.PHONY: freeze

freeze:
	rm -f cabal.project.freeze
	cabal new-freeze
