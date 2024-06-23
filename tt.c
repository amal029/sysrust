#include "example_prog.h"
#include <stdio.h>

int rcal(float x, int y) { return x + y; }

char tick() {
  char b = '\0';
  scanf("%c", &b);
  static int counter = 0;
  counter++;
  if (counter == 3) {
    H_curr.status = 1;
  }
  return b;
}

