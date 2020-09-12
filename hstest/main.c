#include "main.h"
#include <stdio.h>

int main() {
  int argc = 0;
  char* argv[] = { 0 }; // argv must end with NULL

  // Initialize Haskell runtime
  char** args = argv;
  printf("initing...\n");
  hs_init(&argc, &args);
}
