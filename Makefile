all: bin/tp bin/3solve

test:
	prove -e '' -r t

bin/tp: prolog/tp-main.pl prolog/tp.pl bin/3solve
	swipl -q -t main -o bin/tp -c prolog/tp-main.pl

bin/3solve: prolog/3solve-main.pl prolog/3solve.pl prolog/patch.pl
	swipl -q -t main -o bin/3solve -c prolog/3solve-main.pl
