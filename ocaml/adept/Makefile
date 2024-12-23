all: main

main: main.ml moonbase.ml moonbase.mli
	docker run -v $$(pwd):/app -u $$(id -u):$$(id -g) -it --rm ocaml/opam:debian-12-ocaml-4.13 bash -c 'cd /app && ocamlopt -o main moonbase.mli moonbase.ml main.ml'

clean:
	rm main *.o *.cmi *.cmx
