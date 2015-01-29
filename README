Building a patched GHC 7.8 for hermit-syb:

First, you need to update happy and alex to the very latest versions so they generate correct primops.

```
cabal update
cabal install alex
cabal install happy
```

Then, build a modified 7.8:

```
# If on windows, uncomment the following line:
# git config --global core.autocrlf false
# NB: on a mac `curl -O` instead of `wget`
git clone -b ghc-7.8 git://git.haskell.org/ghc.git ghc-7.8
cd ghc-7.8
./sync-all get -b ghc-7.8
cd libraries/base
patch -p1 < path/to/hermit-syb/base.patch
cd ../ghc-prim
patch -p1 < path/to/hermit-syb/ghc-prim.patch
cd ../..
cp mk/build.mk.sample mk/build.mk
```

Now edit mk/build.mk and uncomment: `BuildFlavour = perf`

Now build ghc with the following three commands (and grab drink of your choice):
```
perl boot
./configure
make
```
See: http://hackage.haskell.org/trac/ghc/wiki/Building/Preparation if you have problems here.

Once done, symlink the stage2 compiler.

```
cd inplace/bin
ln -s ghc-stage2 ghc
pwd
```

add the result of `pwd` to the beginning of your PATH. With the symlink, should allow you to use
your existing cabal with your newly compiled GHC HEAD
try: `which ghc` and `ghc -V` to make sure the right one is being used

Now to build HERMIT itself:

```
cabal update
# kure
cabal install kure
# syb
git clone http://michaeldadams.org/projects/hermit/syb.git
cd syb
cabal install
cd ..
# haskell-src
git clone git://github.com/haskell-pkg-janitors/haskell-src.git
cd haskell-src
patch -p1 < path/to/hermit-syb/haskell-src.patch
cabal install
cd ..
# hermit
git clone git@github.com:ku-fpg/hermit.git
cd hermit
cabal install
cd ..
```

Once that is done:

```
cd hermit-syb
cabal install
cd ..

hermit Sum.hs -opt=HERMIT.Optimization.SYB +Main mapInt
- or -
hermit Sum.hs -opt=HERMIT.Optimization.SYB +Main mapIntM
- or -
hermit Sum.hs -opt=HERMIT.Optimization.SYB +Main mapInt mapIntM
```

Note about submodules
=====================

A version of HERMIT that works with this plugin is pegged as
a submodule, in case the most recent HERMIT does not work.
When you first clone this repository, you need to:

git submodule init
git submodule update

to pull down the contents of the submodules.

Whenever you pull new changes that include new submodule hashes, you must:

git submodule update

More information on working with submodules:

http://git-scm.com/book/en/Git-Tools-Submodules
