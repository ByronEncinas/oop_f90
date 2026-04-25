#===============================================
# Directories
#===============================================
SRC_DIR  = src
APP_DIR  = app
OBJ_DIR  = obj
EXEC_DIR = bin
LIB_DIR  = lib
#===============================================
# Compiler
#===============================================
FC = gfortran
AR = ar
ARFLAGS = rcs
#===============================================
# Flags
#===============================================
BASE_FLAGS = -Wall -Wextra -Wpedantic -std=f2008 -I$(OBJ_DIR)
DEBUG_FLAGS = -O0 -g -fcheck=all -fbacktrace
OPT_FLAGS = -O3
FAST_MATH_FLAGS = -O3 -ffast-math -march=native -flto
LAPACK_LIBS = -llapack -lblas
# default build mode
FFLAGS  = $(BASE_FLAGS) $(OPT_FLAGS)
LDFLAGS =
#===============================================
# Source Files and Objects
#===============================================
FSRC     = $(wildcard $(SRC_DIR)/*.f90)
FOBJECTS = $(patsubst $(SRC_DIR)/%.f90, $(OBJ_DIR)/%.o, $(FSRC))
MAIN_SRC = $(APP_DIR)/main.f90
MAIN_OBJ = $(OBJ_DIR)/main.o
EXEC     = $(EXEC_DIR)/oopf90
STATIC_LIB = $(LIB_DIR)/liboopf90.a
#===============================================
# Targets
#===============================================
.PHONY: all lib clean spotless fast lapack fastlapack debug
all: $(EXEC) $(STATIC_LIB)
#-----------------------------------------------
# Static library only
#-----------------------------------------------
lib: $(STATIC_LIB)
#-----------------------------------------------
# Debug build
#-----------------------------------------------
debug: FFLAGS = $(BASE_FLAGS) $(DEBUG_FLAGS)
debug: LDFLAGS =
debug: $(EXEC) $(STATIC_LIB)
#-----------------------------------------------
# Fast-math build
#-----------------------------------------------
fast: FFLAGS = $(BASE_FLAGS) $(FAST_MATH_FLAGS)
fast: LDFLAGS =
fast: $(EXEC) $(STATIC_LIB)
#-----------------------------------------------
# LAPACK/BLAS build
#-----------------------------------------------
lapack: LDFLAGS = $(LAPACK_LIBS)
lapack: $(EXEC) $(STATIC_LIB)
#-----------------------------------------------
# Fast + LAPACK
#-----------------------------------------------
fastlapack: FFLAGS = $(BASE_FLAGS) $(FAST_MATH_FLAGS)
fastlapack: LDFLAGS = $(LAPACK_LIBS)
fastlapack: $(EXEC) $(STATIC_LIB)
#===============================================
# Static library
#===============================================
$(STATIC_LIB): $(FOBJECTS)
	@mkdir -p $(LIB_DIR)
	$(AR) $(ARFLAGS) $@ $^
#===============================================
# Link step
#===============================================
$(EXEC): $(FOBJECTS) $(MAIN_OBJ)
	@mkdir -p $(EXEC_DIR)
	$(FC) $(FFLAGS) -o $@ $^ $(LDFLAGS) -lm
#===============================================
# Compile main
#===============================================
$(MAIN_OBJ): $(MAIN_SRC) $(FOBJECTS)
	@mkdir -p $(OBJ_DIR)
	$(FC) $(FFLAGS) -c $< -o $@ -J$(OBJ_DIR)
#===============================================
# Compile sources
#===============================================
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.f90
	@mkdir -p $(dir $@)
	$(FC) $(FFLAGS) -c $< -o $@ -J$(OBJ_DIR)
#===============================================
# Clean
#===============================================
clean:
	rm -rf $(OBJ_DIR)/*.o $(OBJ_DIR)/*.mod $(EXEC_DIR) $(LIB_DIR) *~ *.pro *.ascii *.nat *.Gh
spotless: clean
	rm -rf $(OBJ_DIR) $(EXEC_DIR) $(LIB_DIR)
