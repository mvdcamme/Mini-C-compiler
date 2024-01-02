int f(int* x) {
	*x = 115;
	return 1;
}

void main(int argc, char argv) {
	int c;
	int *p;
	p = &c;
	c = 1;
	*p = 42;
	f(p);
	print(c);
	print(*p);
	return;
}
