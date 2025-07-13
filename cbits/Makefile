VENDOR = vendor
OBJDIR = .obj
SRC = src
SRCS = $(wildcard $(SRC)/*.c)
CC = gcc

CFLAGS=-mbmi -mbmi2 -mlzcnt -mavx -mavx2 -O3 -fsanitize=address -Wformat
COMPILE.c = $(CC) $(DEPFLAGS) $(CFLAGS) -c
COMPILE_TEST.c = $(CC) $(DEPFLAGS) $(CFLAGS) -I src -I vendor/theft/inc -I vendor/greatest -I vendor/ubench.h -c

# ------------------------------------------------------------------------------
# Automatic recipes for object files

DEPDIR := .deps
DEPFLAGS = -MT $@ -MMD -MP -MF $(DEPDIR)/$*.d

$(OBJDIR)/%.o : %.c
	mkdir -p $(@D)
	mkdir -p .deps/$(patsubst $(OBJDIR)/%,%,$(@D))
	$(COMPILE.c) $(OUTPUT_OPTION) $<

$(OBJDIR)/test/%.o : test/%.c
	mkdir -p $(@D)
	mkdir -p .deps/$(dir $<)
	$(COMPILE_TEST.c) -MF .deps/$<.d $(OUTPUT_OPTION) $<

DEPFILES := $(SRCS:%.c=$(DEPDIR)/%.d) $(TEST_SRCS:%.c=$(DEPDIR)/%.d) $(TEST_UTIL_SRCS:%.c=$(DEPDIR)/%.d)
$(DEPFILES):

include $(wildcard $(DEPFILES))

# ------------------------------------------------------------------------------
# Compile a shared library which can be linked in executables

OBJS = $(patsubst %.c,$(OBJDIR)/%.o,$(SRCS))

$(OBJDIR)/lib.so: $(OBJS)
	$(CC) -o $@ $^ -shared

# ------------------------------------------------------------------------------
# Automatic test dispatch
#
# Running `make test_name` will build and run a test file named `name`
# in the test directory.
#
# Running `make test` will run all tests.

TEST_SRC = test
TEST_BIN = .test_bin
TEST_SRCS := $(wildcard $(TEST_SRC)/*.c)
TEST_SRCS := $(filter-out $(TEST_SRC)/test.c, $(TEST_SRCS))
TEST_OBJS := $(patsubst %.c,$(OBJDIR)/%.o,$(TEST_SRCS))
TEST_INCLUDES = -I src -I vendor/theft/inc -I vendor/greatest -I vendor/ubench.h
TEST_LIBS = -L$(OBJDIR) -l:lib.so -lm vendor/theft/build/libtheft.a
TEST_COMPILE = $(CC) $(CFLAGS) $(TEST_INCLUDES)
RUN_WITH_LIB = LD_LIBRARY_PATH=.obj:$(LD_LIBRARY_PATH)

.PRECIOUS: $(TEST_BIN)/%
$(TEST_BIN)/%: $(OBJDIR)/$(TEST_SRC)/%.o $(OBJDIR)/lib.so | $(TEST_BIN)
	$(TEST_COMPILE) -o $@ $(TEST_LIBS) $<

# Special rule for test.c binary that links all test object files
$(TEST_BIN)/test: $(TEST_SRC)/test.c $(TEST_OBJS) $(OBJDIR)/lib.so | $(TEST_BIN)
	$(TEST_COMPILE) -o $@ $(TEST_SRC)/test.c $(TEST_OBJS) $(TEST_LIBS) 

$(TEST_BIN):
	mkdir -p $@

test: $(TEST_BIN)/test
	@$(RUN_WITH_LIB) ./$(TEST_BIN)/test

# ------------------------------------------------------------------------------
# Automatic bench dispatch
#
# Running `make bench_name` will build and run a bench file named `name`
# in the bench directory.

BENCH_SRC = bench
BENCH_BIN = .bench_bin
BENCH_SRCS = $(wildcard $(BENCH_SRC)/*.c)
BENCH_BINS = $(patsubst $(BENCH_SRC)/%.c,$(BENCH_BIN)/%,$(BENCH_SRCS))

$(BENCH_BIN)/%: $(BENCH_SRC)/%.c $(OBJDIR)/lib.so
	mkdir -p $(BENCH_BIN)
	$(CC) $(CFLAGS) -o $@ -L$(OBJDIR) -l:lib.so -I src -I vendor/ubench.h -lm $(BENCH_SRC)/$*.c

.PRECIOUS: $(BENCH_BIN)/%
bench_%: $(BENCH_BIN)/%
	@LD_LIBRARY_PATH=.obj:$(LD_LIBRARY_PATH) ./.bench_bin/$*


# ------------------------------------------------------------------------------
# Bins

.PHONY: gen_constants
gen_constants: bin/gen_constants.c 
	$(CC) $(CFLAGS) -o gen_constants -I src -lm bin/gen_constants.c
	./gen_constants > src/constants.h

# ------------------------------------------------------------------------------
# Clean

.PHONY: clean
clean:
	rm -rf $(OBJDIR)
	rm -rf $(TEST_BIN)
	rm -rf $(BENCH_BIN)
	rm -rf $(DEPDIR)


# ------------------------------------------------------------------------------
# All

all: $(OBJDIR)/lib.so $(TEST_BIN)/test $(BENCH_BINS)

# ------------------------------------------------------------------------------
# Bear

.PHONY: bear
bear:
	make clean
	bear -- make all
