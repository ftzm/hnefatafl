.DEFAULT_GOAL := all

# Directories
SRC_DIR = src
TEST_DIR = test
BENCH_DIR = bench
BIN_DIR = bin
OBJ_DIR = .obj
TEST_BIN_DIR = .test_bin
BENCH_BIN_DIR = .bench_bin
DEP_DIR = .deps
VENDOR = vendor

# Sources
SRCS := $(wildcard $(SRC_DIR)/*.c)
TEST_SRCS := $(wildcard $(TEST_DIR)/*.c)
TEST_SRCS := $(filter-out $(TEST_DIR)/test.c, $(TEST_SRCS))
BENCH_SRCS := $(wildcard $(BENCH_DIR)/*.c)

# Object files
LIB_OBJS := $(patsubst %.c,$(OBJ_DIR)/%.o,$(SRCS))
TEST_OBJS := $(patsubst %.c,$(OBJ_DIR)/%.o,$(TEST_SRCS))

# Targets
LIB_TARGET := $(OBJ_DIR)/lib.so
BENCH_BINS := $(patsubst $(BENCH_DIR)/%.c,$(BENCH_BIN_DIR)/%,$(BENCH_SRCS))

# Compiler and flags
CC := gcc
CFLAGS := -mbmi -mbmi2 -mlzcnt -mavx -mavx2 -O3 -fsanitize=address -Wformat
LDFLAGS := -fsanitize=address
DEP_FLAGS = -MMD -MP -MT $@ -MF $(DEP_DIR)/$*.d

# Include paths
SRC_INCLUDES := -I $(SRC_DIR)
TEST_INCLUDES := $(SRC_INCLUDES) -I vendor/theft/inc -I vendor/greatest -I vendor/ubench.h
BENCH_INCLUDES := $(SRC_INCLUDES) -I vendor/ubench.h

# Libraries
THEFT_LIB := vendor/theft/build/libtheft.a
TEST_LIBS := -L$(OBJ_DIR) -l:lib.so $(THEFT_LIB) -lm
BENCH_LIBS := -L$(OBJ_DIR) -l:lib.so -lm
RUN_ENV := LD_LIBRARY_PATH=$(OBJ_DIR):$${LD_LIBRARY_PATH:-}

# Link commands
LINK_BASE := $(CC) $(LDFLAGS)

# Directory creation targets
$(OBJ_DIR) $(TEST_BIN_DIR) $(BENCH_BIN_DIR) $(DEP_DIR):
	@mkdir -p $@

$(OBJ_DIR)/$(SRC_DIR) $(OBJ_DIR)/$(TEST_DIR): | $(OBJ_DIR)
	@mkdir -p $@

$(DEP_DIR)/$(SRC_DIR) $(DEP_DIR)/$(TEST_DIR): | $(DEP_DIR)
	@mkdir -p $@

# ------------------------------------------------------------------------------
# Library object files and shared library compilation

# Pattern rule for main library object files
$(OBJ_DIR)/%.o : %.c | $(OBJ_DIR)/$(SRC_DIR) $(DEP_DIR)/$(SRC_DIR)
	@$(CC) $(CFLAGS) -MMD -MP -MT $@ -MF $(DEP_DIR)/$*.d $(SRC_INCLUDES) -c -o $@ $<

# Build the shared library from all library object files
$(LIB_TARGET): $(LIB_OBJS)
	@$(CC) $(LDFLAGS) -o $@ $^ -shared

# ------------------------------------------------------------------------------
# Test object files compilation

# Pattern rule for test object files
$(OBJ_DIR)/$(TEST_DIR)/%.o : $(TEST_DIR)/%.c | $(OBJ_DIR)/$(TEST_DIR) $(DEP_DIR)/$(TEST_DIR)
	@$(CC) $(CFLAGS) -MMD -MP -MT $@ -MF $(DEP_DIR)/$(TEST_DIR)/$*.d $(TEST_INCLUDES) -c -o $@ $<

# ------------------------------------------------------------------------------
# Automatic dependency management
#
# How this works:
# 1. DEP_FLAGS in compilation commands generates .d files with header dependencies
# 2. Each .d file contains a rule like: obj/foo.o: foo.c foo.h bar.h
# 3. We include all .d files to let Make know about header dependencies
# 4. If a header changes, Make will recompile the affected .o files
# 5. The $(DEP_FILES): empty rule prevents errors if .d files don't exist yet

DEP_FILES := $(SRCS:%.c=$(DEP_DIR)/%.d) $(TEST_SRCS:%.c=$(DEP_DIR)/%.d)
$(DEP_FILES):

include $(wildcard $(DEP_FILES))

# ------------------------------------------------------------------------------
# Test executable compilation and execution
#
# Running `make test` will run all tests.

$(TEST_BIN_DIR)/%: $(OBJ_DIR)/$(TEST_DIR)/%.o $(LIB_TARGET) | $(TEST_BIN_DIR)
	@$(CC) $(LDFLAGS) -o $@ $< $(TEST_LIBS)

.PRECIOUS: $(TEST_BIN_DIR)/%

# Special rule for test.c binary that links all test object files
$(TEST_BIN_DIR)/test: $(TEST_DIR)/test.c $(TEST_OBJS) $(LIB_TARGET) | $(TEST_BIN_DIR)
	@$(CC) $(CFLAGS) $(LDFLAGS) $(TEST_INCLUDES) -o $@ $(TEST_DIR)/test.c $(TEST_OBJS) $(TEST_LIBS)

.PHONY: test
test: $(TEST_BIN_DIR)/test
	@$(RUN_ENV) ./$(TEST_BIN_DIR)/test

# ------------------------------------------------------------------------------
# Automatic bench dispatch
#
# Running `make bench_name` will build and run a bench file named `name`
# in the bench directory.

$(BENCH_BIN_DIR)/%: $(BENCH_DIR)/%.c $(LIB_TARGET) | $(BENCH_BIN_DIR)
	@$(CC) $(CFLAGS) $(LDFLAGS) $(BENCH_INCLUDES) -o $@ $< $(BENCH_LIBS)

.PRECIOUS: $(BENCH_BIN_DIR)/%

bench_%: $(BENCH_BIN_DIR)/%
	@$(RUN_ENV) ./$(BENCH_BIN_DIR)/$*
# ------------------------------------------------------------------------------
# Bins

.PHONY: gen_constants
gen_constants: $(BIN_DIR)/gen_constants.c
	@$(CC) $(CFLAGS) $(SRC_INCLUDES) -o gen_constants $(BIN_DIR)/gen_constants.c -lm
	@./gen_constants > $(SRC_DIR)/constants.h || (rm -f gen_constants $(SRC_DIR)/constants.h; exit 1)

# ------------------------------------------------------------------------------
# Clean

.PHONY: clean
clean:
	@rm -rf $(OBJ_DIR)
	@rm -rf $(TEST_BIN_DIR)
	@rm -rf $(BENCH_BIN_DIR)
	@rm -rf $(DEP_DIR)
	@rm -f gen_constants

# ------------------------------------------------------------------------------
# Dependencies

.PHONY: deps
deps:
	@echo "Initializing dependencies..."
	git submodule update --init --recursive
	@if [ ! -f "vendor/theft/build/libtheft.a" ]; then \
		echo "Building theft..."; \
		cd vendor/theft && sed -i '1s|#!/usr/bin/awk -f|#!$(shell which awk) -f|' scripts/mk_bits_lut && make; \
	fi

# ------------------------------------------------------------------------------
# All

.PHONY: all
all: deps $(LIB_TARGET) $(TEST_BIN_DIR)/test $(BENCH_BINS)

# ------------------------------------------------------------------------------
# Bear

.PHONY: bear
bear:
	@$(MAKE) clean
	@bear -- $(MAKE) all
