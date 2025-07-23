# Simple dam break

This is just a learning exercise for me to see how to optimize this code.

## Building

### Dependencies

- Fortran Package Manager 
- A compiler (GNU>=15, latest NVFORTRAN, latest IFX)

I haven't tested non latest nvfortran or ifx, this is why I say "latest".

### How to build

The code is built using the [Fortran package manager](https://fpm.fortran-lang.org/) which
can be installed following the [instructions here](https://fpm.fortran-lang.org/install/index.html).

My recommendation is to install it using `./install.sh --prefix=$HOME/` and then adding the bin to you `$PATH`. 

You can also install it with conda. 

Once the FPM is installed, simply do: `fpm build` and to run `fpm run`

### Executing the code

To install locally and run manually using nvfortran use:

```
fpm install --prefix . --profile release --flag "-O3 -fast -mp=gpu"
```

To get verbose output of the compiler use:

```
fpm install --prefix . --profile release --flag "-O3 -fast -mp=gpu -Minfo=accel,mp" --verbose
```

`install` will create a bin/ directory within your `pic-swe/` directory. You can simply go there and run `./main`

To run without "installing": `fpm run --profile release --flag "-O3 -fast -mp=gpu"

To turn ON/OFF GPU offloading at runtime use the variable `export OMP_TARGET_OFFLOAD=disabled/mandatory`

You can profile a run with `fpm run --runner 'nsys profile --stats=true' --profile release --flag "-O3 -fast -mp=gpu"`

### Unit tests 

The code is unit tested and you can run them via `fpm test` 

## Known issues

Using gcc < 15 will cause the code to fail to compile due to a mismatch in the OpenMP directives. NVFORTRAN and IFX
are the compilers that are best tested as of now. 

## Visualizing your simulation

The code will dump CSVs that detail the dam breaking, you can plot them using the visualize.py script. This is currently 
commented out for the GPU version
