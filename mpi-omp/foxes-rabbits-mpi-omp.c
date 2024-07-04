#include <mpi.h>
#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#define EMPTY -1
#define ROCK 0
#define RABBIT 1
#define FOX 2

#define BLOCK_LOW(id, p, m) ((id)*(m)/(p))
#define BLOCK_HIGH(id, p, m) (BLOCK_LOW((id)+1, p, m) - 1)
#define BLOCK_SIZE(id, p, m) (BLOCK_HIGH(id, p, m) - BLOCK_LOW(id, p, m) + 1)
#define BLOCK_OWNER(i, p, m) (((p)*((i)+1)-1)/(m))

struct square {
	int type; // EMPTY, ROCK, RABBIT or FOX
	int age; // How many generations the animal as lived for, EMPTY if type is not animal
	int starve; // How long until the animal starves, EMPTY if type is not animal
	int moved; // Indicates if the cell as been worked on that subgeneration
};

int gen, m, n, nrock, nrab, rabb, nfox, foxb, starve, seed, id, p;

float r4_uni(uint32_t *seed);
void insert_element(int i, int j, int atype, struct square **map);
void generate_element(int num, int atype, uint32_t *seed, struct square **map);
void populate_map(uint32_t *seed, struct square **map);
struct square ** generate_map();
struct square ** generate_share();
void extend_map_sections(struct square **map);
void exchange_map_shares(struct square **map, struct square **share);
void solve_map_borders(struct square **map, struct square **share);
void conflict_resolution(int a, int b, struct square **map, struct square **share);
void copy_map(int type, struct square **map1, struct square **map2);
void free_map(struct square **map);
void free_share(struct square **share);
void print_map_section(struct square **map);
void print_share(struct square **share);
int * entity_count(struct square **map);
void fox_movement(int i, int j, int newi, int newj, struct square **map1, struct square **map2);
void rabbit_movement(int i, int j, int newi, int newj, struct square **map1, struct square **map2);
void no_movement(int type, int i, int j, struct square **map1, struct square **map2);
void pick_cell(int i, int j, int init, int fin, int *move, struct square **map);
void run_simulation(struct square **map1, struct square **map2, struct square **share);

float r4_uni(uint32_t *seed) {
	int seed_input, sseed;
	float r;

	seed_input = *seed;
	*seed ^= (*seed << 13);
	*seed ^= (*seed >> 17);
	*seed ^= (*seed << 5);
	sseed = *seed;
	r = 0.5 + 0.2328306e-09 * (seed_input + sseed);

	return r;
}

void insert_element(int i, int j, int atype, struct square **map) {
	if (atype == ROCK) {
		map[i][j].type = ROCK;
		map[i][j].age = EMPTY;
		map[i][j].starve = EMPTY;
		map[i][j].moved = 0;
	}
	else if (atype == RABBIT) {
		map[i][j].type = RABBIT;
		map[i][j].age = 0;
		map[i][j].starve = EMPTY;
		map[i][j].moved = 0;
	}
	else if (atype == FOX) {
		map[i][j].type = FOX;
		map[i][j].age = 0;
		map[i][j].starve = starve;
		map[i][j].moved = 0;
	}
	else {
		map[i][j].type = EMPTY;
		map[i][j].age = EMPTY;
		map[i][j].starve = EMPTY;
		map[i][j].moved = 0;
	}
}

void generate_element(int num, int atype, uint32_t *seed, struct square **map) {
	int gi, i, j;

	for (int k = 0; k < num; k++) {
		gi = m * r4_uni(seed);
		j = n * r4_uni(seed);
		if (id == BLOCK_OWNER(gi, p, m)) {
			i = gi - BLOCK_LOW(id, p, m) + 1;
			if (id == 0) {
				i--;
			}
			if (map[i][j].type == EMPTY) {
				insert_element(i, j, atype, map);
			}
		}
	}
}

void populate_map(uint32_t *seed, struct square **map) {
	generate_element(nrock, ROCK, seed, map);
	generate_element(nrab, RABBIT, seed, map);
	generate_element(nfox, FOX, seed, map);
}

struct square ** generate_map() {
	int size;
	struct square **map;

	if (id == 0 || id == p-1) {
		size = BLOCK_SIZE(id, p, m) + 1;
	}
	else {
		size = BLOCK_SIZE(id, p, m) + 2;
	}

	map = (struct square **) malloc((size) * sizeof(struct square*));
	for (int i = 0; i < size; i++) {
		map[i] = (struct square *) malloc(n * sizeof(struct square));
		for (int j = 0; j < n; j++) {
			insert_element(i, j, EMPTY, map);
		}
	}

	return map;
}

struct square ** generate_share() {
	int size;
	struct square **share;

	if (id == 0 || id == p-1) {
		size = 2;
	}
	else {
		size = 4;
	}

	share = (struct square **) malloc((size) * sizeof(struct square*));
	for (int i = 0; i < size; i++) {
		share[i] = (struct square *) malloc(n * sizeof(struct square));
		for (int j = 0; j < n; j++) {
			insert_element(i, j, EMPTY, share);
		}
	}

	return share;
}

void extend_map_sections(struct square **map) {
	MPI_Request request;
	MPI_Status status;

	int count = n*sizeof(struct square)/sizeof(int);

	if (id == 0) {
		MPI_Irecv(map[BLOCK_SIZE(0, p, m)], count, MPI_INT, 1, 123, MPI_COMM_WORLD, &request);
		MPI_Send(map[BLOCK_SIZE(0, p, m)-1], count, MPI_INT, 1, 123, MPI_COMM_WORLD);
		MPI_Wait(&request, &status);
	}
	if (id == 1) {
		MPI_Irecv(map[0], count, MPI_INT, 0, 123, MPI_COMM_WORLD, &request);
		MPI_Send(map[1], count, MPI_INT, 0, 123, MPI_COMM_WORLD);
		MPI_Wait(&request, &status);
	}

	for (int k = 2; k < p; k++) {
		if (id == k-1) {
			MPI_Irecv(map[BLOCK_SIZE(k-1, p, m)+1], count, MPI_INT, k, 123, MPI_COMM_WORLD, &request);
			MPI_Send(map[BLOCK_SIZE(k-1, p, m)], count, MPI_INT, k, 123, MPI_COMM_WORLD);
			MPI_Wait(&request, &status);
		}
		if (id == k) {
			MPI_Irecv(map[0], count, MPI_INT, k-1, 123, MPI_COMM_WORLD, &request);
			MPI_Send(map[1], count, MPI_INT, k-1, 123, MPI_COMM_WORLD);
			MPI_Wait(&request, &status);
		}
	}
}

void exchange_map_shares(struct square **map, struct square **share) {
	MPI_Request request;
	MPI_Status status;

	int count = n*sizeof(struct square)/sizeof(int);

	if (id == 0) {
		MPI_Irecv(share[0], count, MPI_INT, 1, 1234, MPI_COMM_WORLD, &request);
		MPI_Send(map[BLOCK_SIZE(0, p, m)-1], count, MPI_INT, 1, 1234, MPI_COMM_WORLD);
		MPI_Wait(&request, &status);
	}
	if (id == 1) {
		MPI_Irecv(share[0], count, MPI_INT, 0, 1234, MPI_COMM_WORLD, &request);
		MPI_Send(map[0], count, MPI_INT, 0, 1234, MPI_COMM_WORLD);
		MPI_Wait(&request, &status);
	}
	if (id == 0) {
		MPI_Irecv(share[1], count, MPI_INT, 1, 12345, MPI_COMM_WORLD, &request);
		MPI_Send(map[BLOCK_SIZE(0, p, m)], count, MPI_INT, 1, 12345, MPI_COMM_WORLD);
		MPI_Wait(&request, &status);
	}
	if (id == 1) {
		MPI_Irecv(share[1], count, MPI_INT, 0, 12345, MPI_COMM_WORLD, &request);
		MPI_Send(map[1], count, MPI_INT, 0, 12345, MPI_COMM_WORLD);
		MPI_Wait(&request, &status);
	}
	for (int k = 2; k < p; k++) {
		if (id == k-1) {
			MPI_Irecv(share[2], count, MPI_INT, k, 1234, MPI_COMM_WORLD, &request);
			MPI_Send(map[BLOCK_SIZE(k-1, p, m)], count, MPI_INT, k, 1234, MPI_COMM_WORLD);
			MPI_Wait(&request, &status);
		}
		if (id == k) {
			MPI_Irecv(share[0], count, MPI_INT, k-1, 1234, MPI_COMM_WORLD, &request);
			MPI_Send(map[0], count, MPI_INT, k-1, 1234, MPI_COMM_WORLD);
			MPI_Wait(&request, &status);
		}
		if (id == k-1) {
			MPI_Irecv(share[3], count, MPI_INT, k, 12345, MPI_COMM_WORLD, &request);
			MPI_Send(map[BLOCK_SIZE(k-1, p, m)+1], count, MPI_INT, k, 12345, MPI_COMM_WORLD);
			MPI_Wait(&request, &status);
		}
		if (id == k) {
			MPI_Irecv(share[1], count, MPI_INT, k-1, 12345, MPI_COMM_WORLD, &request);
			MPI_Send(map[1], count, MPI_INT, k-1, 12345, MPI_COMM_WORLD);
			MPI_Wait(&request, &status);
		}
	}
}

void solve_map_borders(struct square **map, struct square **share) {
	if (id != 0 && id != p-1) {
		conflict_resolution(BLOCK_SIZE(id, p, m), 2, map, share);
	}
	if (id == 0) {
		conflict_resolution(BLOCK_SIZE(id, p, m)-1, 0, map, share);
	}
	else {
		conflict_resolution(0, 0, map, share);
	}
}

void conflict_resolution(int a, int b, struct square **map, struct square **share) {
	for (int i = 0; i < 2; i++) {
		for (int j = 0; j < n; j++) {
			if (share[b+i][j].moved) {
				if (!map[a+i][j].moved) {
					map[a+i][j].type = share[b+i][j].type;
					map[a+i][j].age = share[b+i][j].age;
					map[a+i][j].starve = share[b+i][j].starve;
					map[a+i][j].moved = 1;
				}
				else {
					if (share[b+i][j].type == RABBIT) {
						if (map[a+i][j].type == EMPTY) {
							map[a+i][j].type = RABBIT;
							map[a+i][j].age = share[b+i][j].age;
						}
						else if (map[a+i][j].type == RABBIT) {
							if (map[a+i][j].age < share[b+i][j].age) {
								map[a+i][j].age = share[b+i][j].age;
							}
						}
						else if (map[a+i][j].type == FOX) {
							map[a+i][j].starve = starve;
						}
					}
					else if (share[b+i][j].type == FOX) {
						if (map[a+i][j].type == EMPTY) {
							map[a+i][j].type = FOX;
							map[a+i][j].age = share[b+i][j].age;
							map[a+i][j].starve = share[b+i][j].starve;
						}
						else if (map[a+i][j].type == RABBIT) {
							map[a+i][j].type = FOX;
							map[a+i][j].age = share[b+i][j].age;
							map[a+i][j].starve = starve;
						}
						else if (map[a+i][j].type == FOX) {
							if ((map[a+i][j].age < share[b+i][j].age) || (map[a+i][j].age == share[b+i][j].age && map[a+i][j].starve < share[b+i][j].starve)) {
								map[a+i][j].type = FOX;
								map[a+i][j].age = share[b+i][j].age;
								map[a+i][j].starve = share[b+i][j].starve;
							}
						}
					}
				}
			}
		}
	}
}

void copy_map(int type, struct square **map1, struct square **map2) {
	int size;

	if (id == 0 || id == p-1) {
		size = BLOCK_SIZE(id, p, m) + 1;
	}
	else {
		size = BLOCK_SIZE(id, p, m) + 2;
	}

	// removes fox if it should die at the end of the generation and sets move flags to 0
	if(type){
		for (int i = 0; i < size; i++) {
			for (int j = 0; j < n; j++) {
				map2[i][j].moved = 0;
				if (map2[i][j].type == FOX && map2[i][j].starve <= 0) {
					map2[i][j].type = EMPTY;
					map2[i][j].age = EMPTY;
					map2[i][j].starve = EMPTY;
				}
			}
		}
	}

	// copies the map
	for (int i = 0; i < size; i++) {
		memcpy(map1[i], map2[i], n*sizeof(struct square));
	}
}

void free_map(struct square **map) {
	int size;

	if (id == 0 || id == p-1) {
		size = BLOCK_SIZE(id, p, m) + 1;
	}
	else {
		size = BLOCK_SIZE(id, p, m) + 2;
	}

	for (int i = 0; i < size; i++) {
		free(map[i]);
	}
	free(map);
}

void free_share(struct square **share) {
	int size;

	if (id == 0 || id == p-1) {
		size = 2;
	}
	else {
		size = 4;
	}

	for (int i = 0; i < size; i++) {
		free(share[i]);
	}
	free(share);
}

void print_map_section(struct square **map) {
	int size;

	if (id == 0 || id == p-1) {
		size = BLOCK_SIZE(id, p, m) + 1;
	}
	else {
		size = BLOCK_SIZE(id, p, m) + 2;
	}

	printf("\nP%d.", id);
	for (int j = 0; j < n; j++) {
		printf("...");
	}
	printf("\n   ");
	for (int j = 0; j < n; j++) {
		printf("%.2d|", j);
	}
	printf("\n");
	for (int i = 0; i < size; i++) {
		printf("%.2d:", i);
		for (int j = 0; j < n; j++) {
			if (map[i][j].type == EMPTY) {
				printf("  |");
			}
			else if (map[i][j].type == ROCK) {
				printf(" *|");
			}
			else if (map[i][j].type == RABBIT) {
				printf(" R|");
			}
			else {
				printf(" F|");
			}
		}
		printf("\n");
	}
	for (int j = 0; j <= n; j++) {
		printf("---");
	}
	printf("\n");
}

void print_share(struct square **share) {
	int size;

	if (id == 0 || id == p-1) {
		size = 2;
	}
	else {
		size = 4;
	}

	printf("\nP%d.", id);
	for (int j = 0; j < n-1; j++) {
		printf("...");
	}
	printf("\n");
	for (int i = 0; i < size; i++) {
		for (int j = 0; j < n; j++) {
			if (share[i][j].type == EMPTY) {
				printf("  |");
			}
			else if (share[i][j].type == ROCK) {
				printf(" *|");
			}
			else if (share[i][j].type == RABBIT) {
				printf(" R|");
			}
			else {
				printf(" F|");
			}
		}
		printf("\n");
	}
	for (int j = 0; j < n; j++) {
		printf("...");
	}
	printf("\n");
}

int * entity_count(struct square **map) {
	int size, init;
	static int counter[3] = {0, 0, 0};

	if (id == 0) {
		size = BLOCK_SIZE(id, p, m);
		init = 0;
	}
	else if (id == p-1) {
		size = BLOCK_SIZE(id, p, m) + 1;
		init = 1;
	}
	else {
		size = BLOCK_SIZE(id, p, m) + 1;
		init = 1;
	}

	for (int i = init; i < size; i++) {
		for (int j = 0; j < n; j++) {
			if (map[i][j].type == ROCK) {
				counter[0]++;
			}
			else if (map[i][j].type == RABBIT) {
				counter[1]++;
			}
			else if (map[i][j].type == FOX) {
				counter[2]++;
			}
		}
	}

	return counter;
}

void fox_movement(int i, int j, int newi, int newj, struct square **map1, struct square **map2) {
	map2[i][j].moved = 1;
	// movement of a fox to an empty cell
	if (map2[newi][newj].type == EMPTY) {
		#pragma omp atomic write
		map2[newi][newj].type = FOX;
		#pragma omp atomic write
		map2[newi][newj].moved = 1;
		#pragma omp atomic write
		map2[newi][newj].starve = map1[i][j].starve - 1;
		// movement of a fox to an empty cell if it breeds
		if (map1[i][j].age + 1 >= foxb) {
			#pragma omp atomic write
			map2[newi][newj].age = 0;
			map2[i][j].age = 0;
			map2[i][j].starve = starve;
		}
		// movement of a fox to an empty cell if it doesn't breed
		else {
			#pragma omp atomic write
			map2[newi][newj].age = map1[i][j].age + 1;
			map2[i][j].type = EMPTY;
			map2[i][j].age = EMPTY;
			map2[i][j].starve = EMPTY;
		}
	}
	// movement of a fox to a rabbit cell
	else if (map2[newi][newj].type == RABBIT) {
		#pragma omp atomic write
		map2[newi][newj].type = FOX;
		#pragma omp atomic write
		map2[newi][newj].starve = starve;
		#pragma omp atomic write
		map2[newi][newj].moved = 1;
		// movement of a fox to a rabbit cell if it breeds
		if (map1[i][j].age + 1 >= foxb) {
			#pragma omp atomic write
			map2[newi][newj].age = 0;
			map2[i][j].type = FOX;
			map2[i][j].age = 0;
			map2[i][j].starve = starve;
		}
		// movement of a fox to a rabbit cell if it doesn't breed
		else {
			#pragma omp atomic write
			map2[newi][newj].age = map1[i][j].age + 1;
			map2[i][j].type = EMPTY;
			map2[i][j].age = EMPTY;
			map2[i][j].starve = EMPTY;
		}
	}
	// movement of a fox to a fox cell
	else if (map2[newi][newj].type == FOX) {
		// movement of a fox to a fox cell if it breeds
		if (map1[i][j].age + 1 >= foxb) {
			map2[i][j].type = FOX;
			map2[i][j].age = 0;
			map2[i][j].starve = starve;
			if (map2[newi][newj].age == 0 && map1[i][j].starve - 1 > map2[newi][newj].starve) {
				#pragma omp atomic write
				map2[newi][newj].starve = map1[i][j].starve - 1;
			}
		}
		// movement of a fox to a fox cell if it doesn't breed
		else {
			map2[i][j].type = EMPTY;
			map2[i][j].age = EMPTY;
			map2[i][j].starve = EMPTY;
			if (map1[i][j].age + 1 > map2[newi][newj].age) {
				#pragma omp atomic write
				map2[newi][newj].age = map1[i][j].age + 1;
				if (map2[newi][newj].starve != starve) {
					#pragma omp atomic write
					map2[newi][newj].starve = map1[i][j].starve - 1;
				}
			}
			else if (map1[i][j].age + 1 == map2[newi][newj].age && map1[i][j].starve - 1 > map2[newi][newj].starve) {
				#pragma omp atomic write
				map2[newi][newj].starve = map1[i][j].starve - 1;
			}
		}
	}
}

void rabbit_movement(int i, int j, int newi, int newj, struct square **map1, struct square **map2) {
	map2[i][j].moved = 1;
	// movement of a rabbit to an empty cell
	if (map2[newi][newj].type == EMPTY) {
		#pragma omp atomic write
		map2[newi][newj].type = RABBIT;
		#pragma omp atomic write
		map2[newi][newj].moved = 1;
		// movement of a rabbit to an empty cell if it breeds
		if (map1[i][j].age + 1 >= rabb) {
			#pragma omp atomic write
			map2[newi][newj].age = 0;
			map2[i][j].age = 0;
		}
		// movement of a rabbit to an empty cell if it doesn't breed
		else {
			#pragma omp atomic write
			map2[newi][newj].age = map1[i][j].age + 1;
			map2[i][j].type = EMPTY;
			map2[i][j].age = EMPTY;
		}
	}
	// movement of a rabbit to a fox cell
	else if (map2[newi][newj].type == FOX) {
		#pragma omp atomic write
		map2[newi][newj].starve = starve;
		// movement of a rabbit to a fox cell if it breeds
		if (map1[i][j].age + 1 >= rabb) {
			map2[i][j].age = 0;
		}
		// movement of a rabbit to a fox cell if it doesn't breed
		else {
			map2[i][j].type = EMPTY;
			map2[i][j].age = EMPTY;
		}
	}
	// movement of a rabbit to a rabbit cell
	else if (map2[newi][newj].type == RABBIT) {
		// movement of a rabbit to a rabbit cell if it breeds
		if (map1[i][j].age + 1 >= rabb) {
			map2[i][j].age = 0;
		}
		// movement of a rabbit to a rabbit cell if it doesn't breed
		else {
			map2[i][j].type = EMPTY;
			map2[i][j].age = EMPTY;
			if (map1[i][j].age + 1 > map2[newi][newj].age) {
				#pragma omp atomic write
				map2[newi][newj].age = map1[i][j].age + 1;
			}
		}
	}
}

void no_movement(int type, int i, int j, struct square **map1, struct square **map2) {
	if (!type || !map2[i][j].moved) {
		map2[i][j].moved = 1;
		map2[i][j].age = map1[i][j].age + 1;
		if (map1[i][j].type == FOX) {
			map2[i][j].starve = map1[i][j].starve - 1;
		}
	}
}

void pick_cell(int i, int j, int init, int fin, int *move, struct square **map) {
	int s;
	int l = 0;
	int c = (BLOCK_LOW(id, p, m)+i-init) * n + j;
	int choices[4] = {-1, -1, -1, -1};

	// if it's a rock, empty, or it has moved previous subgeneration, no movement is needed
	if (map[i][j].type == EMPTY || map[i][j].type == ROCK || map[i][j].moved) {
		move[0] = 0;
		move[1] = -1;
		move[2] = -1;
		return;
	}

	// check movement conditions for a rabbit
	if (map[i][j].type == RABBIT) {
		if (i > 0 && map[i-1][j].type == EMPTY) {
			choices[0] = l;
			l++;
		}
		if (j < n-1 && map[i][j+1].type == EMPTY) {
			choices[1] = l;
			l++;
		}
		if (i < fin-1 && map[i+1][j].type == EMPTY) {
			choices[2] = l;
			l++;
		}
		if (j > 0 && map[i][j-1].type == EMPTY) {
			choices[3] = l;
			l++;
		}
	}

	// check movement conditions for a fox
	else if (map[i][j].type == FOX) {
		if (i > 0 && map[i-1][j].type == RABBIT) {
			choices[0] = l;
			l++;
		}
		if (j < n-1 && map[i][j+1].type == RABBIT) {
			choices[1] = l;
			l++;
		}
		if (i < fin-1 && map[i+1][j].type == RABBIT) {
			choices[2] = l;
			l++;
		}
		if (j > 0 && map[i][j-1].type == RABBIT) {
			choices[3] = l;
			l++;
		}
		// if there's no rabbit nearby
		if (!l) {
			if (i > 0 && map[i-1][j].type == EMPTY) {
				choices[0] = l;
				l++;
			}
			if (j < n-1 && map[i][j+1].type == EMPTY) {
				choices[1] = l;
				l++;
			}
			if (i < fin-1 && map[i+1][j].type == EMPTY) {
				choices[2] = l;
				l++;
			}
			if (j > 0 && map[i][j-1].type == EMPTY) {
				choices[3] = l;
				l++;
			}
		}
	}

	// case where the animal shouldn't move
	if (!l) {
		move[0] = 0;
		move[1] = -1;
		move[2] = -1;
		return;
	}

	// case where it should move, pick the cell to move to
	s = c % l;
	if (choices[0] == s) {
		move[0] = 1;
		move[1] = i-1;
		move[2] = j;
		return;
	}
	if (choices[1] == s) {
		move[0] = 1;
		move[1] = i;
		move[2] = j+1;
		return;
	}
	if (choices[2] == s) {
		move[0] = 1;
		move[1] = i+1;
		move[2] = j;
		return;
	}
	if (choices[3] == s) {
		move[0] = 1;
		move[1] = i;
		move[2] = j-1;
		return;
	}
}

void run_simulation(struct square **map1, struct square **map2, struct square **share) {
	int i, j, newi, newj, size, init, fin, run;
	int move[3];

	#pragma omp parallel private(run, i, j, newi, newj, move) if(BLOCK_SIZE(id, p, m) > 5)
	if (id == 0) {
		init = 0;
		size = BLOCK_SIZE(id, p, m);
		fin = size + 1;
	}
	else if (id == p-1) {
		init = 1;
		size = BLOCK_SIZE(id, p, m) + 1;
		fin = size;
	}
	else {
		init = 1;
		size = BLOCK_SIZE(id, p, m) + 1;
		fin = size + 1;
	}

	// runs for selected number of generations
	for (run = 0; run < gen; run++) {
		// subgeneration red
		#pragma omp for private(j, newi, newj, move)
		for (i = init; i < size; i++) {
			for (j = (BLOCK_LOW(id, p, m)+i-init)%2; j < n; j += 2) {
				pick_cell(i, j, init, fin, move, map1);
				//the animal should move
				if (move[0]) {
					newi = move[1];
					newj = move[2];
					// a fox is going to move
					if (map1[i][j].type == FOX) {
						fox_movement(i, j, newi, newj, map1, map2);
					}
					// a rabbit is going to move
					else {
						rabbit_movement(i, j, newi, newj, map1, map2);
					}
				}
				// the animal should not move or the cell is not an animal
				else {
					no_movement(0, i, j, map1, map2);
				}
			}
		}

		exchange_map_shares(map2, share);
		solve_map_borders(map2, share);
		copy_map(0, map1, map2);

		// subgeneration black
		#pragma omp for private(j, newi, newj, move)
		for (i = init; i < size; i++) {
			for (j = (BLOCK_LOW(id, p, m)+i+1-init)%2; j < n; j += 2) {
				pick_cell(i, j, init, fin, move, map1);
				//the animal should move
				if (move[0]) {
					newi = move[1];
					newj = move[2];
					// a fox is going to move
					if (map1[i][j].type == FOX) {
						fox_movement(i, j, newi, newj, map1, map2);
					}
					// a rabbit is going to move
					else {
						rabbit_movement(i, j, newi, newj, map1, map2);
					}
				}
				// the animal should not move or the cell is not an animal
				else {
					no_movement(1, i, j, map1, map2);
				}
			}
		}

		exchange_map_shares(map2, share);
		solve_map_borders(map2, share);
		copy_map(1, map1, map2);
	}
}

int main(int argc, char *argv[]) {
	int *counter, *received, k;
	double time;

	MPI_Init(&argc, &argv);
	MPI_Comm_size(MPI_COMM_WORLD, &p);
	MPI_Comm_rank(MPI_COMM_WORLD, &id);
	MPI_Request request;
	MPI_Status status;

	// checks if number of arguments is correct
	if (argc > 11) {
		fprintf(stderr, "Too many arguments supplied.\n");
		exit(-1);
	}
	else if (argc < 11) {
		fprintf(stderr, "Too few arguments supplied.\n");
		exit(-1);
	}

	// receives arguments from command line
	gen = atoi(argv[1]);
	m = atoi(argv[2]);
	n = atoi(argv[3]);
	nrock = atoi(argv[4]);
	nrab = atoi(argv[5]);
	rabb = atoi(argv[6]);
	nfox = atoi(argv[7]);
	foxb = atoi(argv[8]);
	starve = atoi(argv[9]);
	seed = atoi(argv[10]);

	// checks if the problem is big enough for the number of processes
	if (p*2 > m) {
		fprintf(stderr, "Map too small for number of processes.\n");
		exit(-1);
	}

	// checks if there's at least two processes running
	if (p < 2) {
		fprintf(stderr, "Requires at least two processes.\n");
		exit(-1);
	}

	struct square **map1 = generate_map();
	struct square **map2 = generate_map();
	struct square **share = generate_share();
	populate_map(&seed, map1);
	extend_map_sections(map1);
	copy_map(0, map2, map1);

	time =- omp_get_wtime();
    run_simulation(map1, map2, share);
	time += omp_get_wtime();

	counter = entity_count(map2);

	if (id != 0) {
		MPI_Send(counter, 3, MPI_INT, 0, 1234, MPI_COMM_WORLD);
	}

	if (id == 0) {
		received = malloc (3 * sizeof(int));
		for(k=1; k<p; k++){			
			MPI_Irecv(received, 3, MPI_INT, k, 1234, MPI_COMM_WORLD, &request);
			MPI_Wait(&request, &status);
			counter[0]=counter[0]+received[0];
			counter[1]=counter[1]+received[1];
			counter[2]=counter[2]+received[2];			
		}
		free(received);
	}

	if (id == 0) {
		fprintf(stdout, "%d %d %d\n", counter[0], counter[1], counter[2]);
		//fprintf(stderr, "%.1fs\n", time);
	}

	free_map(map1);
	free_map(map2);
	free_share(share);

	MPI_Finalize();

	return 0;
}
