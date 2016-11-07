all: check doc
.PHONY: doc

# build package documentation
doc:
	R -e 'devtools::document()'
check:
	R -e 'devtools::check()'
install:
	R -e 'devtools::install()'
