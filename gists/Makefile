#===============================================
# Compiler and Flags
#===============================================
FC = gfortran
FFLAGS = -O3 -Wall -Wextra -Wpedantic -std=f2008

#===============================================
# Directories
#===============================================
SRC_DIR = src
OBJ_DIR = obj
EXEC_DIR = bin

#===============================================
# Source Files and Objects
#===============================================
FSRC = $(wildcard $(SRC_DIR)/*.f90)	# Collect all .f90 files from the src/ directory
FOBJECTS = $(patsubst $(SRC_DIR)/%.f90, $(OBJ_DIR)/%.o, $(FSRC))	# Create object file names from source

#===============================================
# General Parameters
#===============================================
EXEC = $(EXEC_DIR)/fst

#===============================================
# Compilation rules
#===============================================
.PHONY: all clean spotless

all: $(EXEC)

clean:
	rm -rf *~ $(OBJ_DIR)/*.o $(OBJ_DIR)/*.mod *.pro *.ascii *.nat *.Gh *.o ./bin

spotless: clean
	rm -rf *~ $(OBJ_DIR)/*.o $(OBJ_DIR)/*.mod $(OBJ_DIR)/ $(EXEC)

$(EXEC): $(FOBJECTS)
	@mkdir -p $(EXEC_DIR)	# Ensure the bin directory exists
	$(FC) $(FFLAGS) -o $@ $^ -lm

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.f90
	@mkdir -p $(dir $@)	# Ensure the obj directory structure exists
	$(FC) $(FFLAGS) -c $< -o $@ -J$(OBJ_DIR)
