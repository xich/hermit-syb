GHC_PATH=$(HOME)/ghc/sybghc/inplace/bin/ghc

all : Main.hs
	ghc --make -O1 -fforce-recomp Main

run:
	./Main -c $(GHC_PATH) -f "-O2 -ddump-simpl" -r -K1g

time:
	./Main -c $(GHC_PATH) -f "-O2 -ddump-simpl" -r -K1g -z

profile:
	./Main -c $(GHC_PATH) -f -O2 -r -K1g -p

space:
	./Main -c $(GHC_PATH) -f "-O2 -ddump-simpl +RTS -hT -RTS" -r "-K1g -hT"

final:
	./Main -n 10 -c $(GHC_PATH) -f "-O2 -ddump-simpl" -r -K1g

finaltime:
	./Main -n 10 -c $(GHC_PATH) -f "-O2 -ddump-simpl" -r -K1g -z
