sbcl := sbcl --noinform --non-interactive --load "load.lisp" --eval
script := script

all:
	$(sbcl) '(main "$(script)")'

clean:
	rm -f *~ *.fasl *.ppm *.png
