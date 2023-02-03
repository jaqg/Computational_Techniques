# Exercise 1 - Roots finding #

## Compilation ##

- To compile, simply type
```bash
$ make
```
and
```bash
./main
```
as `main` is the main executable.

- And, to clean the secondary files (binaries, objects, ...) type
```bash
$ make clean
```

## Input ##

The input can be modified in the file `data/input.data`. **Note**: don't alter
the order of the lines, just change the value of the data.

Once the program is compiled, you can change the input data and type `./main`
again. There's no need to recompile.

## Output ##

The output can be printed in the screen, saved in a file, or both, as the user
desire. The output file is `data/output.dat`.

## Structure of the code ##
The code is structure as:

- The main file, `main.f90`, where the subprograms are called and used
- The modules folder, `modules/`. In this folder there are two modules:
    - `io.f90`: a general module for I/O reading/writing and declaration of
        variables, interfaces, etc.
    - `mymodule.f90`: a module specific to the problem, containing the required
        subprograms to solve the problem.
