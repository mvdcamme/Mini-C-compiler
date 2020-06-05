void main(int argc, char argv) {
	int c;
	int *p;
	p = &c;
	*p = 100;
	print(c);
	return;
}
