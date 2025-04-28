/* #include "microwave.h" */
#include <stdio.h>

int rcal(float x, int y) { return x + y; }

int get_op(float t, int m) {return 0;}
int get_param(float k, int j) {return 2;}


char tick(int wash_status) {
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

