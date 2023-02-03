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

Since the output is pretty short, it is printed in the screen.

## Structure of the code ##
The code is structure as:

- The main file, `main.f90`, where the subprograms are called and used
- The modules folder, `modules/`. In this folder there are two modules:
    - `io.f90`: a general module for I/O reading/writing and declaration of
        variables, interfaces, etc.
    - `mymodule.f90`: a module specific to the problem, containing the required
        subprograms to solve the problem.
