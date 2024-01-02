int f(int x) {
	return x * 10;
}

int g(int x, int y) {
	return f(x) + y;
}

int main(int argc, char argv) {
	int sum;
	sum = g(1, 2);
	print(sum);
	return 0;
}
