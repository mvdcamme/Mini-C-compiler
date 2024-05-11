int array[10];

int main(int argc, char argv) {
	int i;
	int sum;
	for (i = 0; i < 10; i++) {
		array[i] = i;
	};
	for (i = 0; i < 10; i++) {
		sum += array[i];
	};
	print(sum);
	return 0;
}
