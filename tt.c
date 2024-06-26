/* #include "microwave.h" */
#include <stdio.h>

int rcal(float x, int y) { return x + y; }

char tick() {
  char b = '\0';
  scanf("%c", &b);
  /* static int counter = 0; */
  /* counter++; */
  /* if (counter == 3) { */
  /*   A_curr.status = 1; */
  /*   B_curr.status = 1; */
  /* } */
  /* if (counter == 5){ */
  /*   R_curr.status = 1; */
  /*   counter = 0; */
  /* } */
  return b;
}

