int g;

void f(int* x) {
	*x = 23 + g;
	return;
}

void main(int argc, char argv) {
	int c;
	int *p;
	p = &g;
	*p = 115;
	for (g = g; g > 0; g--) {
		print(g);
	};
	return;
}
