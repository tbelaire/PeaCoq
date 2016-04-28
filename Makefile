all: example.html

/Users/tbelaire/.local/bin/coq-annotate : src/Coq-Annotate.hs
	stack install

example-with-context.v : example.v /Users/tbelaire/.local/bin/coq-annotate
	coq-annotate example.v > example-with-context.v

example-with-js.html : example-with-context.v coq-lit.py
	python3 coq-lit.py  example-with-context.v -weave > example-with-js.html

example.html : example-with-js.html template.html
	cat template.html example-with-js.html > example.html

