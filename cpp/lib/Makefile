CFLAGS=-mbmi -mbmi2 -mlzcnt -mavx -mavx2

.PHONY: clean

layer.o: layer.c layer.h
	gcc -c layer.c -o layer.o $(CFLAGS)

board.o: board.c board.h
	gcc -o board.o -c board.c $(CFLAGS)

capture.o: capture.c capture.h
	gcc -o capture.o -c capture.c $(CFLAGS)

io.o: io.c io.h
	gcc -o io.o -c io.c $(CFLAGS)

move.o: move.c move.h
	gcc -o move.o -c move.c $(CFLAGS)

test: test.c layer.o board.o io.o
	gcc -o test test.c layer.o board.o io.o

build_test_capture: test_capture.c layer.o board.o io.o capture.o
	@echo target is $@, source is $<
	@echo deps: $^
	gcc -o test_capture test_capture.c layer.o board.o io.o capture.o

build_test_move: test_move.c layer.o board.o io.o capture.o move.o
	@echo target is $@, source is $<
	@echo deps: $^
	gcc -o test_move $^

test_capture: build_test_capture
	./test_capture

build_bench_capture: bench_capture.c layer.o board.o io.o capture.o
	gcc -o bench_capture $^

bench_capture: build_bench_capture
	time ./bench_capture

gen_constants: gen_constants.c layer.o io.o
	gcc -o gen_constants $^

clean:
	rm *.o
