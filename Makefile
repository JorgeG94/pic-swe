# -------------------------------
# PIC-SWE simple Makefile
# -------------------------------
# Usage examples:
#   make                          # uses defaults
#   make FC=ifx FFLAGS="-O3 -qopenmp" LDFLAGS="-qopenmp"
#   make clean / make veryclean
#
# Directory layout (Makefile at repo root):
#   src/
#     deps/    pic.f90 pic_types.F90 pic_logger.f90 pic_string_utils.f90 pic_timer.F90 pic_constants.f90
#     grid/    pic_grid_2d.f90
#              pic_gpu_utilities.f90 pic_swe_constants.f90 pic_state.f90 pic_flux_2d.f90
#              pic_timestep.f90 pic_boundaries.f90 pic_update_2d.f90 pic_shallow_water_driver.f90
#   app/
#     main.f90
#
# Output:
#   build/lib/libpic_fpm.a
#   main
# -------------------------------

# Toolchain (override on command line if desired)
FC      ?= gfortran
AR      ?= ar
ARFLAGS ?= rcs

# Flags (override on command line if desired)
FFLAGS  ?= -O3 -fopenmp --offload-arch=gfx90a 
LDFLAGS ?=

# Tree
SRC_DIR  := src
DEPS_DIR := $(SRC_DIR)/deps
GRID_DIR := $(SRC_DIR)/grid
APP_DIR  := app

BUILD_DIR := build
OBJ_DIR   := $(BUILD_DIR)/obj
MOD_DIR   := $(BUILD_DIR)/mod
LIB_DIR   := $(BUILD_DIR)/lib

LIB_NAME := libpic_fpm.a
LIB      := $(LIB_DIR)/$(LIB_NAME)
EXE      := main

# Include & module dirs
INC := -I$(MOD_DIR) -I$(SRC_DIR) -I$(DEPS_DIR) -I$(GRID_DIR)
MOD := -J$(MOD_DIR)

# ---- Order-sensitive object list for the library ----
# (matches the order you provided)
LIB_OBJS := \
  $(OBJ_DIR)/deps/pic.o \
  $(OBJ_DIR)/deps/pic_types.o \
  $(OBJ_DIR)/pic_gpu_utilities.o \
  $(OBJ_DIR)/pic_swe_constants.o \
  $(OBJ_DIR)/grid/pic_grid_2d.o \
  $(OBJ_DIR)/deps/pic_string_utils.o \
  $(OBJ_DIR)/deps/pic_constants.o \
  $(OBJ_DIR)/deps/pic_timer.o \
  $(OBJ_DIR)/deps/pic_logger.o \
  $(OBJ_DIR)/pic_state.o \
  $(OBJ_DIR)/pic_flux_2d.o \
  $(OBJ_DIR)/pic_timestep.o \
  $(OBJ_DIR)/pic_boundaries.o \
  $(OBJ_DIR)/pic_update_2d.o \
  $(OBJ_DIR)/pic_shallow_water_driver.o

APP_OBJ := $(OBJ_DIR)/app_main.o

# Convenience phony targets
.PHONY: all clean veryclean print

all: $(EXE)

# ----- Linking the final executable -----
$(EXE): $(LIB) $(APP_OBJ) | $(BUILD_DIR)
	$(FC) $(FFLAGS) -o $@ $(APP_OBJ) $(LIB) $(LDFLAGS)

# ----- Build the static library -----
$(LIB): $(LIB_OBJS) | $(LIB_DIR)
	$(AR) $(ARFLAGS) $@ $(LIB_OBJS)

# ----- Compile rules -----
# .f90 sources (module outputs go to $(MOD_DIR))
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.f90 | $(OBJ_DIR) $(MOD_DIR)
	@mkdir -p $(@D)
	$(FC) $(FFLAGS) $(INC) $(MOD) -c $< -o $@

# .F90 sources (often preprocessed)
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.F90 | $(OBJ_DIR) $(MOD_DIR)
	@mkdir -p $(@D)
	$(FC) $(FFLAGS) -cpp $(INC) $(MOD) -c $< -o $@

# App main
$(APP_OBJ): $(APP_DIR)/main.f90 | $(OBJ_DIR) $(MOD_DIR)
	@mkdir -p $(@D)
	$(FC) $(FFLAGS) $(INC) $(MOD) -c $< -o $@

# ----- Dirs -----
$(BUILD_DIR) $(OBJ_DIR) $(MOD_DIR) $(LIB_DIR):
	@mkdir -p $@

# ----- Utilities -----
print:
	@echo "FC      = $(FC)"
	@echo "FFLAGS  = $(FFLAGS)"
	@echo "LDFLAGS = $(LDFLAGS)"
	@echo "AR      = $(AR)"
	@echo "ARFLAGS = $(ARFLAGS)"
	@echo "OBJ_DIR = $(OBJ_DIR)"
	@echo "MOD_DIR = $(MOD_DIR)"
	@echo "LIB     = $(LIB)"

clean:
	@$(RM) -r $(OBJ_DIR) $(MOD_DIR)

veryclean: clean
	@$(RM) -r $(LIB_DIR) $(BUILD_DIR) $(EXE)

