#include <stdio.h>

int add(int a, int b);
int f(int x, int, int, int, int, int, int, int, int);

int main() {
  int answer;
  answer = add(40, f(1, 2, 3, 4, 5, 6, 7, 8, 9));
  printf("%i\n", answer);
  return 0;
}

int add(int a, int b) {
  int result = a + b;
  return result;
}

int f(int x, int y, int z, int xx, int yy, int zz, int xxx, int yyy, int zzz) {
	int d = x;
	int e = y;
	int f = z;
	int g = d + e + 3;
	return g;
}