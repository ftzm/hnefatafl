TARGET = board
#LIBS = -lm
# CC = gcc
CC = clang
CFLAGS = -O3 -mbmi -mbmi2 -mlzcnt -mavx -mavx2

board: 

board_2: 

run_board_2: board_2
	./board_2

test_capture: board.c tests/capture.c
	$(CC) $(CFLAGS) tests/capture.c -Wall -o $@

run_test_capture: test_capture
	./test_capture

clean:
	-rm -f *.o
#-rm -f $(TARGET)
	find . -type f -executable -delete


# https://stackoverflow.com/questions/1484817/how-do-i-make-a-simple-makefile-for-gcc-on-linux
# .PHONY: default all clean
# 
# default: $(TARGET)
# all: default
# 
# OBJECTS = $(patsubst %.c, %.o, $(wildcard *.c))
# HEADERS = $(wildcard *.h)
# 
# %.o: %.c $(HEADERS)
# 	$(CC) $(CFLAGS) -c $< -o $@
# 
# .PRECIOUS: $(TARGET) $(OBJECTS)
# 
# $(TARGET): $(OBJECTS)
# 	$(CC) $(OBJECTS) -Wall $(LIBS) -o $@
# 
# capture_tests: capture
