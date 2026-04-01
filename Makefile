#===============================================
# Compiler
#===============================================

FC = gfortran

#===============================================
# Directories
#===============================================

SRC_DIR  = src
APP_DIR  = app
OBJ_DIR  = obj
EXEC_DIR = bin

#===============================================
# Flags
#===============================================

FFLAGS = -O3 -Wall -Wextra -Wpedantic -std=f2008 -I$(OBJ_DIR)

#===============================================
# Source Files and Objects
#===============================================

FSRC     = $(wildcard $(SRC_DIR)/*.f90)
FOBJECTS = $(patsubst $(SRC_DIR)/%.f90, $(OBJ_DIR)/%.o, $(FSRC))

MAIN_SRC = $(APP_DIR)/main.f90
MAIN_OBJ = $(OBJ_DIR)/main.o

#===============================================
# General Parameters
#===============================================

EXEC = $(EXEC_DIR)/oopf

#===============================================
# Compilation rules
#===============================================
.PHONY: all clean spotless

all: $(EXEC)

clean:
	rm -rf *~ $(OBJ_DIR)/*.o $(OBJ_DIR)/*.mod *.pro *.ascii *.nat *.Gh *.o ./bin
spotless: clean
	rm -rf *~ $(OBJ_DIR)/*.o $(OBJ_DIR)/*.mod $(OBJ_DIR)/ $(EXEC)
$(EXEC): $(FOBJECTS) $(MAIN_OBJ)
	@mkdir -p $(EXEC_DIR)
	$(FC) $(FFLAGS) -o $@ $^ -lm
$(MAIN_OBJ): $(MAIN_SRC) $(FOBJECTS)
	@mkdir -p $(OBJ_DIR)
	$(FC) $(FFLAGS) -c $< -o $@ -J$(OBJ_DIR)
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.f90
	@mkdir -p $(dir $@)
	$(FC) $(FFLAGS) -c $< -o $@ -J$(OBJ_DIR)
