VENDOR = vendor
OBJDIR = .obj
SRC = src
SRCS = $(wildcard $(SRC)/*.c)

CFLAGS=-mbmi -mbmi2 -mlzcnt -mavx -mavx2 -O3
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

TEST_SRC = test
TEST_BIN = .test_bin

$(TEST_BIN)/%: $(TEST_SRC)/%.c $(OBJDIR)/lib.so
	$(CC) $(CFLAGS) -o $@ -L$(OBJDIR) -l:lib.so -I src -I vendor/theft/inc -I vendor/ubench.h -lm $(TEST_SRC)/$*.c $(OBJDIR)/libtheft.a 

.PRECIOUS: $(TEST_BIN)/%
test_%: $(TEST_BIN)/%
	@LD_LIBRARY_PATH=.obj:$(LD_LIBRARY_PATH) ./.test_bin/$*


# ------------------------------------------------------------------------------
# Bins

gen_constants: bin/gen_constants.c $(OBJDIR)/lib.so
	$(CC) $(CFLAGS) -o gen_constants -L$(OBJDIR) -l:lib.so -I src bin/gen_constants.c

# ------------------------------------------------------------------------------
# Clean

.PHONY: clean
clean:
	rm -r $(OBJDIR)

# ------------------------------------------------------------------------------
# Bear

bear:
	bear -- make test_move
