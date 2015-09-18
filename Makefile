all: bin/tp bin/3solve

bin/tp: prolog/tp-main.pl prolog/tp.pl
	swipl -q -t main -o bin/tp -c prolog/tp-main.pl

bin/3solve: prolog/3solve-main.pl prolog/3solve.pl prolog/diff.pl
	swipl -q -t main -o bin/3solve -c prolog/3solve-main.pl
