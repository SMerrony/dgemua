## Build
* MV/Emua: `gprbuild -Pmvemua.gpr`
* VS/Emua: `gprbuild -Pvsemua.gpr`

Default build is with debugging and without optimisation.  Append `-Xmode=release` for optimised build.

## Clean
`gprclean mvemua.gpr`

## Regenerate tests
`gnattest -P vsemua.gpr  -v  dg_types.ads --harness-dir=driver`

## Recompile tests
`cd obj/driver`

`gprbuild -Ptest_driver`

## Run Tests
(In obj/driver)

`./test_runner`

## Valgrind (Call Profiling)
`valgrind --tool=callgrind ./vsemua -pr SPIGOT.PR -root /home/steve/ada/github.com/dgemua/FILESYSTEM -dir :SAMPLES`

## Run VS/Emua
* `./vsemua -pr SPIGOT.PR -root /home/steve/ada/github.com/dgemua/FILESYSTEM -dir :SAMPLES`
* `./vsemua -pr MMM.PR -root /home/steve/ada/github.com/dgemua/FILESYSTEM -dir :GAMES:MMM`
