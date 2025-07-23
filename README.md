# Simple dam break

This is just a learning exercise for me to see how to optimize this code.

## Building

The code is built using the [Fortran package manager](https://fpm.fortran-lang.org/) which
can be installed following the [instructions here](https://fpm.fortran-lang.org/install/index.html).

Once the FPM is installed, simply do: `fpm build` and to run `fpm run`

You can profile a run with `fpm run --runner 'nsys profile --stats=true' --profile release --flag "-O3 -fast -mp=gpu"`

## Known issues

Using gcc < 15 will cause the code to fail to compile due to a mismatch in the OpenMP directives. NVFORTRAN and IFX
are the compilers that are best tested as of now. 

## Visualizing your simulation

The code will dump CSVs that detail the dam breaking, you can plot them using the visualize.py script
