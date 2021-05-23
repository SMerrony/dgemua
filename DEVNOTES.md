## Build
* MV/Emua: `gprbuild -Pmvemua.gpr`
* VS/Emua: `gprbuild -Pvsemua.gpr`

Default build is with debugging and without optimisation.  Append `-Xmode=release` for optimised build.

## Regenerate tests
`gnattest -P vsemua.gpr  -v  dg_types.ads --harness-dir=driver`

## Recompile tests
`cd obj/driver`

`gprbuild -Ptest_driver`

## Run Tests
(In obj/driver)

`./test_runner`
