#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#define EMPTY -1
#define ROCK 0
#define RABBIT 1
#define FOX 2

struct square{
  int type; // -1:empty | 0:rock | 1:rabbit | 2:fox
  int age; // counts up
  int starve; // counts down
  int moved; // indicates if the animal as moved in the first subgeneration
};

int gen, m, n, nrock, nrab, rabb, nfox, foxb, starve, seed;

float r4_uni(uint32_t *seed);
void insert_animal(int i, int j, int atype, struct square **map);
void generate_element(int num, int atype, uint32_t *seed, struct square **map);
struct square ** generate_world();
void populate_world(uint32_t *seed, struct square **map);
void copy_world(int type, struct square **map1, struct square **map2);
void pick_cell(int i, int j, int * move, struct square **map);
int * count(struct square **map);
void fox_movement(int i, int j, int newi, int newj, struct square **map1, struct square **map2);
void rabbit_movement(int i, int j, int newi, int newj, struct square **map1, struct square **map2);
void no_movement(int type, int i, int j, struct square **map1, struct square **map2);
void run_simulation(struct square **map1, struct square **map2);
void clean_map(struct square **map1, struct square **map2);

float r4_uni(uint32_t *seed){
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

void insert_animal(int i, int j, int atype, struct square **map){
  if(atype == ROCK){
    map[i][j].type = ROCK;
    map[i][j].age = EMPTY;
    map[i][j].starve = EMPTY;
    map[i][j].moved = EMPTY;
  }
  else if(atype == RABBIT){
    map[i][j].type = RABBIT;
    map[i][j].age = 0;
    map[i][j].starve = EMPTY;
    map[i][j].moved = 0;
  }
  else if(atype == FOX){
    map[i][j].type = FOX;
    map[i][j].age = 0;
    map[i][j].starve = starve;
    map[i][j].moved = 0;
  }
  else{
    map[i][j].type = EMPTY;
    map[i][j].age = EMPTY;
    map[i][j].starve = EMPTY;
    map[i][j].moved = EMPTY;
  }
}

void generate_element(int num, int atype, uint32_t *seed, struct square **map){
  int i, j, k;

  for(k = 0; k < num; k++){
		i = m * r4_uni(seed);
		j = n * r4_uni(seed);
		if(map[i][j].type == EMPTY){
    	insert_animal(i, j, atype, map);
    }
  }
}

struct square ** generate_world(){
  int i, j;
  struct square **map = (struct square **) malloc(m * sizeof(struct square*));

  for(i = 0; i < m; i++){
    map[i] = (struct square *) malloc(n * sizeof(struct square));
    for(j = 0; j < n; j++){
      map[i][j].type = EMPTY;
      map[i][j].age = EMPTY;
      map[i][j].starve = EMPTY;
      map[i][j].moved = EMPTY;
    }
  }

  return map;
}

void populate_world(uint32_t *seed, struct square **map){
  generate_element(nrock, ROCK, seed, map);
  generate_element(nrab, RABBIT, seed, map);
  generate_element(nfox, FOX, seed, map);
}

void copy_world(int type, struct square **map1, struct square **map2){
  int i, j;

  if(type){
    #pragma omp for private(j)
    for(i = 0; i < m; i++){
      for(j = 0; j < n; j++){
        map2[i][j].moved = 0;
        if(map2[i][j].type == FOX && map2[i][j].starve <= 0){
          map2[i][j].type = EMPTY;
          map2[i][j].age = EMPTY;
          map2[i][j].starve = EMPTY;
          map2[i][j].moved = EMPTY;
        }
      }
      memcpy(map1[i],map2[i], n * sizeof(struct square));
    }
  }
  else{
    #pragma omp for private(j)
    for(i = 0; i < m; i++){
      memcpy(map1[i],map2[i], n * sizeof(struct square));
    }
  }
}

void pick_cell(int i, int j, int * move, struct square **map){
  int choices[4] = {-1, -1, -1, -1};
  int p = 0;
  int c = i * n + j;
  int s;

  // if it's a rock, is empty, or it has moved previous subgeneration, no movement is needed
  if(map[i][j].type == EMPTY || map[i][j].type == ROCK || map[i][j].moved){
    move[0] = 0;
    move[1] = -1;
    move[2] = -1;
    return;
  }

  // check movement conditions for a rabbit
  if(map[i][j].type == RABBIT){
    if(i > 0 && map[i-1][j].type == EMPTY){
      choices[0] = p;
      p++;
    }
    if(j < n-1 && map[i][j+1].type == EMPTY){
      choices[1] = p;
      p++;
    }
    if(i < m-1 && map[i+1][j].type == EMPTY){
      choices[2] = p;
      p++;
    }
    if(j > 0 && map[i][j-1].type == EMPTY){
      choices[3] = p;
      p++;
    }
  }

  // check movement conditions for a fox
  else if(map[i][j].type == FOX){
    if(i > 0 && map[i-1][j].type == RABBIT){
      choices[0] = p;
      p++;
    }
    if(j < n-1 && map[i][j+1].type == RABBIT){
      choices[1] = p;
      p++;
    }
    if(i < m-1 && map[i+1][j].type == RABBIT){
      choices[2] = p;
      p++;
    }
    if(j > 0 && map[i][j-1].type == RABBIT){
      choices[3] = p;
      p++;
    }
    // if there's no rabbit nearby
    if(!p){
      if(i > 0 && map[i-1][j].type == EMPTY){
        choices[0] = p;
        p++;
      }
      if(j < n-1 && map[i][j+1].type == EMPTY){
        choices[1] = p;
        p++;
      }
      if(i < m-1 && map[i+1][j].type == EMPTY){
        choices[2] = p;
        p++;
      }
      if(j > 0 && map[i][j-1].type == EMPTY){
        choices[3] = p;
        p++;
      }
    }
  }

  // case where the animal shouldn't move
  if(!p){
    move[0] = 0;
    move[1] = -1;
    move[2] = -1;
    return;
  }

  // case where it should move, pick the cell to move to
  s = c % p;
  if(choices[0] == s){
    move[0] = 1;
    move[1] = i-1;
    move[2] = j;
    return;
  }
  if(choices[1] == s){
    move[0] = 1;
    move[1] = i;
    move[2] = j+1;
    return;
  }
  if(choices[2] == s){
    move[0] = 1;
    move[1] = i+1;
    move[2] = j;
    return;
  }
  if(choices[3] == s){
    move[0] = 1;
    move[1] = i;
    move[2] = j-1;
    return;
  }
}

int * count(struct square **map){
  int i, j;
  static int num[3] = {0, 0, 0};

  for(i = 0; i < m; i++){
    for(j = 0; j < n; j++){
      if(map[i][j].type == ROCK)
        num[0]++;
      else if(map[i][j].type == RABBIT)
        num[1]++;
      else if(map[i][j].type == FOX)
        num[2]++;
    }
  }

  return num;
}

void fox_movement(int i, int j, int newi, int newj, struct square **map1, struct square **map2){
  // movement of a fox to an empty cell
  if(map2[newi][newj].type == EMPTY){
    #pragma omp atomic write
    map2[newi][newj].type = FOX;
    #pragma omp atomic write
    map2[newi][newj].moved = 1;
    #pragma omp atomic write
    map2[newi][newj].starve = map1[i][j].starve - 1;
    // movement of a fox to an empty cell if it breeds
    if(map1[i][j].age + 1 >= foxb){
      #pragma omp atomic write
      map2[newi][newj].age = 0;
      map2[i][j].type = FOX;
      map2[i][j].age = 0;
      map2[i][j].starve = starve;
      map2[i][j].moved = 0;
    }
    // movement of a fox to an empty cell if it doesn't breed
    else{
      #pragma omp atomic write
      map2[newi][newj].age = map1[i][j].age + 1;
      map2[i][j].type = EMPTY;
      map2[i][j].age = EMPTY;
      map2[i][j].starve = EMPTY;
      map2[i][j].moved = EMPTY;
    }
  }
  // movement of a fox to a rabbit cell
  else if(map2[newi][newj].type == RABBIT){
    #pragma omp atomic write
    map2[newi][newj].type = FOX;
    #pragma omp atomic write
    map2[newi][newj].starve = starve;
    #pragma omp atomic write
    map2[newi][newj].moved=1;
    // movement of a fox to a rabbit cell if it breeds
    if(map1[i][j].age + 1 >= foxb){
      #pragma omp atomic write
      map2[newi][newj].age = 0;
      map2[i][j].type = FOX;
      map2[i][j].age = 0;
      map2[i][j].starve = starve;
      map2[i][j].moved = 0;
    }
    // movement of a fox to a rabbit cell if it doesn't breed
    else{
      #pragma omp atomic write
      map2[newi][newj].age = map1[i][j].age + 1;
      map2[i][j].type = EMPTY;
      map2[i][j].age = EMPTY;
      map2[i][j].starve = EMPTY;
      map2[i][j].moved = EMPTY;
    }
  }
  // movement of a fox to a fox cell
  else if(map2[newi][newj].type == FOX){
    // movement of a fox to a fox cell if it breeds
    if(map1[i][j].age + 1 >= foxb){
      map2[i][j].type = FOX;
      map2[i][j].age = 0;
      map2[i][j].starve = starve;
      map2[i][j].moved = 0;
      if(map2[newi][newj].age == 0 && map1[i][j].starve - 1 > map2[newi][newj].starve){
        #pragma omp atomic write
        map2[newi][newj].starve = map1[i][j].starve - 1;
      }
    }
    // movement of a fox to a fox cell if it doesn't breed
    else{
      map2[i][j].type = EMPTY;
      map2[i][j].age = EMPTY;
      map2[i][j].starve = EMPTY;
      map2[i][j].moved = EMPTY;
      if(map1[i][j].age + 1 > map2[newi][newj].age){
        #pragma omp atomic write
        map2[newi][newj].age = map1[i][j].age + 1;
        if(map2[newi][newj].starve != starve){
          #pragma omp atomic write
          map2[newi][newj].starve = map1[i][j].starve - 1;
        }
      }
      else if(map1[i][j].age + 1 == map2[newi][newj].age && map1[i][j].starve - 1 > map2[newi][newj].starve){
        #pragma omp atomic write
        map2[newi][newj].starve = map1[i][j].starve - 1;
      }
    }
  }
}

void rabbit_movement(int i, int j, int newi, int newj, struct square **map1, struct square **map2){
  // movement of a rabbit to an empty cell
  if(map2[newi][newj].type == EMPTY){
    #pragma omp atomic write
    map2[newi][newj].type = RABBIT;
    #pragma omp atomic write
    map2[newi][newj].moved = 1;
    #pragma omp atomic write
    map2[newi][newj].starve = EMPTY;
    // movement of a rabbit to an empty cell if it breeds
    if(map1[i][j].age + 1 >= rabb){
      #pragma omp atomic write
      map2[newi][newj].age = 0;
      map2[i][j].type = RABBIT;
      map2[i][j].age = 0;
      map2[i][j].starve = EMPTY;
      map2[i][j].moved = 0;
    }
    // movement of a rabbit to an empty cell if it doesn't breed
    else{
      #pragma omp atomic write
      map2[newi][newj].age = map1[i][j].age + 1;
      map2[i][j].type = EMPTY;
      map2[i][j].age = EMPTY;
      map2[i][j].starve = EMPTY;
      map2[i][j].moved = EMPTY;
    }
  }
  // movement of a rabbit to a fox cell
  else if(map2[newi][newj].type == FOX){
    #pragma omp atomic write
    map2[newi][newj].starve = starve;
    // movement of a rabbit to a fox cell if it breeds
    if(map1[i][j].age + 1 >= rabb){
      map2[i][j].type = RABBIT;
      map2[i][j].age = 0;
      map2[i][j].starve = EMPTY;
      map2[i][j].moved = 0;
    }
    // movement of a rabbit to a fox cell if it doesn't breed
    else{
      map2[i][j].type = EMPTY;
      map2[i][j].age = EMPTY;
      map2[i][j].starve = EMPTY;
      map2[i][j].moved = EMPTY;
    }
  }
  // movement of a rabbit to a rabbit cell
  else if(map2[newi][newj].type == RABBIT){
    // movement of a rabbit to a rabbit cell if it breeds
    if(map1[i][j].age + 1 >= rabb){
      map2[i][j].type = RABBIT;
      map2[i][j].age = 0;
      map2[i][j].starve = EMPTY;
      map2[i][j].moved = 0;
    }
    // movement of a rabbit to a rabbit cell if it doesn't breed
    else{
      map2[i][j].type = EMPTY;
      map2[i][j].age = EMPTY;
      map2[i][j].starve = EMPTY;
      map2[i][j].moved = EMPTY;
      if(map1[i][j].age + 1 > map2[newi][newj].age){
        #pragma omp atomic write
        map2[newi][newj].age = map1[i][j].age + 1;
      }
    }
  }
}

void no_movement(int type, int i, int j, struct square **map1, struct square **map2){
  if(!type || !map2[i][j].moved){
    map2[i][j].age = map1[i][j].age + 1;
    if(map1[i][j].type == FOX){
      map2[i][j].starve = map1[i][j].starve - 1;
    }
  }
}

void run_simulation(struct square **map1, struct square **map2){
  int run, i, j, newi, newj;
  int move[3];

  #pragma omp parallel private(run, i, j, newi, newj, move) if(m > 15)
  {
    // runs for the selected number of generations
    for(run = 0; run < gen; run++){
      // subgeneration red
      #pragma omp for private(j, newi, newj, move)
      for(i = 0; i < m; i++){
        for(j = i%2; j < n; j += 2){
          pick_cell(i, j, move, map1);
          // the animal should move
          if(move[0]){
            newi = move[1];
            newj = move[2];
            // movement of a fox
            if(map1[i][j].type == FOX){
              fox_movement(i, j, newi, newj, map1, map2);
            }
            // movement of a rabbit
            else{
              rabbit_movement(i, j, newi, newj, map1, map2);
            }
          }
          // the animal shouldn't move or the cell is not an animal
          else{
            no_movement(0, i, j, map1, map2);
          }
        }
      }

      copy_world(0, map1, map2);

      // subgeneration black
      #pragma omp for private(j, newi, newj, move)
      for(i = 0; i < m; i++){
        for(j = (i+1)%2; j < n; j += 2){
          pick_cell(i, j, move, map1);
          // the animal should move
          if(move[0]){
            newi = move[1];
            newj = move[2];
            // movement of a fox
            if(map1[i][j].type == FOX){
              fox_movement(i, j, newi, newj, map1, map2);
            }
            // movement of a rabbit
            else{
              rabbit_movement(i, j, newi, newj, map1, map2);
            }
          }
          // the animal shouldn't move or the cell is not an animal
          else{
            no_movement(1, i, j, map1, map2);
          }
        }
      }

      copy_world(1, map1, map2);
    }
  }
}

void clean_map(struct square **map1, struct square **map2){
  for(int i = 0; i < m; i++){
    free(map1[i]);
    free(map2[i]);
  }
  free(map1);
  free(map2);
}

int main(int argc, char *argv[]){
  int *numbers;

  // checks if number of arguments is correct
  if(argc > 11) {
    printf("\nToo many arguments supplied.\n");
    exit(0);
  }
  else if(argc < 11){
    printf("\nToo few arguments supplied.\n");
    exit(0);
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

  double exec_time;

  // create two copies of the map
  struct square **map1 = generate_world();
  struct square **map2 = generate_world();

  populate_world(&seed, map1);
  copy_world(0, map2, map1);

  exec_time = -omp_get_wtime();
  run_simulation(map1, map2);
  exec_time += omp_get_wtime();

  numbers = count(map2);

  //fprintf(stderr, "%.1fs\n", exec_time);
  fprintf(stdout, "%d %d %d\n", numbers[0], numbers[1], numbers[2]);

  clean_map(map1, map2);

  return 0;
}
