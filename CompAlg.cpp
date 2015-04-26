//============================================================================
// Name        : CompAlg.cpp
// Author      : Seriously
// Version     :
// Copyright   : Your copyright notice
// Description : Hello World in C, Ansi-style
//============================================================================

#include <stdio.h>
#include <stdlib.h>
#include <algorithm> // max
#include <math.h> //log2, ceil
#include <bitset>
#include <string>

using namespace std;

#define bitsetSize 32

int gridMul(bitset<bitsetSize> x, bitset<bitsetSize> y);
int karatsuba(bitset<bitsetSize> x, bitset<bitsetSize> y);
int binLength(bitset<bitsetSize> n);
bitset<bitsetSize> convertToBitset(int n);

// set size for the used bitsets. 32 as integers are also 32 bit.
int main(void) {

	int x = 849;
	int y = 649;

	int z = 0;

	const int j = 32;
	bitset<j> jo = bitset<j>(x);

	bitset<bitsetSize> x_bin = convertToBitset(x);
	bitset<bitsetSize> y_bin = convertToBitset(y);
	bitset<bitsetSize> t_bin = x_bin & y_bin;
	int t = gridMul(x_bin, y_bin);
//	int t = pow(3,4);

	string sx = x_bin.to_string();
	string sy = y_bin.to_string();
	string st = t_bin.to_string();

	printf("x_bin = %s\n", sx.c_str());
	printf("y_bin = %s\n", sy.c_str());
	printf("t_bin = %s\n", st.c_str());
	printf("x = %d\n", x);
	printf("y = %d\n", y);
	printf("x*y: \t%d\n", t);
	printf("CHECK: \t%d\n", x*y);
	printf("z = %d\n", z);

	return EXIT_SUCCESS;
}

int gridMul(bitset<bitsetSize> x, bitset<bitsetSize> y){
	int z = 0;
	for(int i = 0; i<bitsetSize; ++i){
		if(x[i] != 0)
			for(int j = 0; j < bitsetSize; ++j){
//				(1 << (i+j) = 1 * 2^(i+j)
				z += y[j] * (1 << (i+j));
			}
	}
	return z;
}

int karatsuba(bitset<bitsetSize> x, bitset<bitsetSize> y) {
//	determine max binary length of x and y. k is the amount of bits to store the max length
	int k = binLength(max(binLength(x), binLength(y)));
	if(k==0)
		return gridMul(x,y);
//	B = 2^2^(k-1)
	int B = 1 << (1 << (k-1));
//	bitset<(1<<(k-1))> x1 =

	return k;
}

//int karatsuba(int x, int y){
////	determine binary length of x and y. 32 as integers are stored in 32 bit.
//	int lx = 32 - __builtin_clz(x);
//	int ly = 32 - __builtin_clz(y);
//	int maxl = max(lx,ly);
//	int k = 32 - __builtin_clz(maxl);
//	if(k==0)
//		return x * y;
////	k-2 because -1 from algorithm and again -1 because this is shift not exp.
//	int B = 2 << ((2 << k-2)-1);
//
//
//	return k;
//}

inline int binLength(bitset<bitsetSize> n) {
	for (int i = bitsetSize - 1; i >= 0; --i) {
		if (n[i] == 1)
			return i + 1;
	}
	return 0;
}

inline bitset<bitsetSize> convertToBitset(int n) {
	bitset<bitsetSize> n_binary = bitset<bitsetSize>(n);
	return n_binary;
}
