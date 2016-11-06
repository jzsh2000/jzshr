all: doc
.PHONY: doc

# build package documentation
doc:
	R -e 'devtools::check();devtools::document()'
