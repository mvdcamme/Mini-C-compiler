int a;
char b;
int x;

int g(char c) {
	return 5;
}

int f(int a, int b) {
	int c;
	x = 10;
	a = 1;
	if (a) {
		print(a);
		return a + 2;
	} else {
		return 1;
	}
}

int main(int argc, char argv) {
	print(f(1, 2));
	return 0;
}
