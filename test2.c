#include <stdio.h>

int add(int a, int b);

int main() {
  int answer;
  answer = add(40, 2);
	printf("%i\n", answer);
  return 0;
}

int add(int a, int b) {
  int result = a + b;
  return result;
}
