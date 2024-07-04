all:
	gcc -O3 foxes-rabbits.c -o foxes-rabbits -fopenmp

clean:
	rm foxes-rabbits
