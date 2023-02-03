# This is a template of makefile for a Fortran library, with the
# present makefile and Fortran source files in several
# directories. This makefile should be in a directory related to the
# source directories.

# This is a makefile for GNU make.

# 1. Source directories and source files

# Write relative paths separated by blanks (do not include . in the
# list):
VPATH = ...

# Or, if there are many directories:
VPATH = $(shell cat directories)

# Write source file names, without path, separated by blanks, not
# commas. Only .[fF] files, not included files.
sources = ...

# Or, if there are many files, they may be listed in a separate file,
# one file name per line:
sources := $(shell cat file_list)
# If you want a sorted list, sort the file instead of using sort at
# each run of make.

# 2. Objects and libraries

objects := $(addsuffix .o, $(basename ${sources}))
# Or, if all the source files have the same suffix, more simply:
objects := $(sources:.f=.o)

lib_dyn = lib....so
lib_stat = lib....a

ARFLAGS = rvU

# 3. Compiler-dependent part

FC = gfortran
CPPFLAGS = ...

FFLAGS = -ffree-form -fpic
# (you need at least the option for free source form)

# 4. Rules

%.so:
	$(FC) $(LDFLAGS) -shared -fpic $^ -o $@

.PHONY: all clean depend

all: ${lib_stat}
# (should be the first rule)
# We do not include ${lib_dyn} in all, because usually we do not want it

${lib_dyn}: ${objects}
${lib_stat}: ${lib_stat}(${objects})

depend depend.mk:
	makedepf90 -D... -free -Wmissing -Wconfused $(addprefix -I, ${VPATH}) -nosrc -u ... ${sources} >depend.mk
# (See whether you need the "-D" and "-u" options.)

clean:
	rm -f ${lib_dyn} ${lib_stat} ${objects}

include depend.mk
