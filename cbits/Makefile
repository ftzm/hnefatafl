VENDOR = vendor
OBJDIR = .obj
TEST_SRC = test
TEST_BIN = .test_bin
SRC = src
SRCS = $(wildcard $(SRC)/*.c)
OBJS = $(SRCS:.c=.o)

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

$(OBJDIR)/lib.so: $(OBJS)
	$(CC) -o $@ $^ -shared

# ------------------------------------------------------------------------------
# Automatic test dispatch
#
# Running `make test_name` will build and run a test file named `name`
# in the test directory.

$(TEST_BIN)/%: $(TEST_SRC)/%.c $(OBJDIR)/lib.so
	$(CC) -o $@ -L$(OBJDIR) -l:lib.so -I src $(TEST_SRC)/$*.c

test_%: $(TEST_BIN)/%
	LD_LIBRARY_PATH=.obj:$(LD_LIBRARY_PATH) ./.test_bin/$*
