all: main

main: Main.hs Moonbase.hs
	docker run -v $$(pwd):/app  -it --rm haskell:9.6.6 bash -c 'cd /app && stack ghc --resolver=lts-22.43 --package random -- -package random -O2 -o main Main.hs'

clean:
	rm main *.o *.hi
