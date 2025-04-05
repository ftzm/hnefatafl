VENDOR = vendor
OBJDIR = .obj
SRC = src
SRCS = $(wildcard $(SRC)/*.c)

CFLAGS=-mbmi -mbmi2 -mlzcnt -mavx -mavx2
COMPILE.c = $(CC) $(DEPFLAGS) $(CFLAGS) -c

# ------------------------------------------------------------------------------
# Automatic recipes for object files

DEPDIR := .deps
DEPFLAGS = -MT $@ -MMD -MP -MF $(DEPDIR)/$*.d

$(OBJDIR)/%.o : %.c $(DEPDIR)/%.d
	mkdir -p $(basename $(@D))
	mkdir -p .deps/$(patsubst $(OBJDIR)/%,%,$(basename $(@D)))
	$(COMPILE.c) $(OUTPUT_OPTION) $<

DEPFILES := $(SRCS:%.c=$(DEPDIR)/%.d)
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
TEST_SRCS = $(wildcard $(TEST_SRC)/*.c)
TEST_BINS = $(patsubst $(TEST_SRC)/%.c,$(TEST_BIN)/%,$(TEST_SRCS))

.PRECIOUS: $(TEST_BIN)/%
$(TEST_BIN)/%: $(TEST_SRC)/%.c $(OBJDIR)/lib.so
	mkdir -p $(TEST_BIN)
	$(CC) $(CFLAGS) -o $@ -L$(OBJDIR) -l:lib.so -I src -I vendor/theft/inc -I vendor/greatest -I vendor/ubench.h -lm $(TEST_SRC)/$*.c vendor/theft/build/libtheft.a 

test_%: $(TEST_BIN)/%
	@LD_LIBRARY_PATH=.obj:$(LD_LIBRARY_PATH) ./.test_bin/$*

test: $(TEST_BINS)
	@for bin in $(TEST_BINS); do \
	  LD_LIBRARY_PATH=.obj:$(LD_LIBRARY_PATH) ./$$bin; \
	done

# ------------------------------------------------------------------------------
# Automatic bench dispatch
#
# Running `make bench_name` will build and run a bench file named `name`
# in the bench directory.

BENCH_SRC = bench
BENCH_BIN = .bench_bin

$(BENCH_BIN)/%: $(BENCH_SRC)/%.c $(OBJDIR)/lib.so
	mkdir -p $(BENCH_BIN)
	$(CC) $(CFLAGS) -o $@ -L$(OBJDIR) -l:lib.so -I src -I vendor/ubench.h -lm $(BENCH_SRC)/$*.c

.PRECIOUS: $(BENCH_BIN)/%
bench_%: $(BENCH_BIN)/%
	@LD_LIBRARY_PATH=.obj:$(LD_LIBRARY_PATH) ./.bench_bin/$*


# ------------------------------------------------------------------------------
# Bins

.PHONY: gen_constants
gen_constants: bin/gen_constants.c $(OBJDIR)/lib.so
	$(CC) $(CFLAGS) -o gen_constants -L$(OBJDIR) -l:lib.so -I src -lm bin/gen_constants.c
	LD_LIBRARY_PATH=.obj:$LD_LIBRARY_PATH ./gen_constants

# ------------------------------------------------------------------------------
# Clean

.PHONY: clean
clean:
	rm -rf $(OBJDIR)
	rm -rf $(TEST_BIN)
	rm -rf $(BENCH_BIN)
	rm -rf $(DEPDIR)

# ------------------------------------------------------------------------------
# Bear

bear:
	bear -- make test_move
