/*
 * Exercise4_v1.cpp
 *
 *  Created on: 26 Apr 2015
 *      Author: seriously
 */

#include <stdio.h>
#include <stdlib.h>
#include <algorithm> // max
#include <math.h> //log2, ceil
#include <bitset>
//#include <string>

using namespace std;

//#define DEBUG
#define bitsetSize 32

bitset<bitsetSize> karatsuba(bitset<bitsetSize> x, bitset<bitsetSize> y);
inline int gridMul(bitset<bitsetSize> x, bitset<bitsetSize> y);
inline int binLength(bitset<bitsetSize> n);
/**
 * copies bits out of a bitset and places the pattern at the beginning of a new set. incl. "start" and "end"
 */
inline bitset<bitsetSize> copyBits(bitset<bitsetSize> set, int start, int end);
/**
 * copies bits out of a bitset source and places the pattern at the position pos of a set target. incl. "start" and "end"
 */
inline bitset<bitsetSize> copyBitsTo(bitset<bitsetSize> source, int start,
		int end, bitset<bitsetSize> target, int pos);
inline bitset<bitsetSize> binAdd(bitset<bitsetSize> a, bitset<bitsetSize> b);
inline bitset<bitsetSize> binSub(bitset<bitsetSize> a, bitset<bitsetSize> b);

// set size for the used bitsets. 32 as integers are also 32 bit.
int main(void) {

	int x = 42;
	int y = -12;

	bitset<bitsetSize> x_bin(x);
	bitset<bitsetSize> y_bin(y);
	bitset<bitsetSize> z_bin = karatsuba(x_bin, y_bin);

//
	printf("x_bin \t= %s\n", x_bin.to_string().c_str());
	printf("y_bin \t= %s\n", y_bin.to_string().c_str());
	printf("z_bin \t= %s\n", z_bin.to_string().c_str());
	printf("x \t= %d\n", x);
	printf("y \t= %d\n", y);
	printf("z \t= %lu\n", z_bin.to_ulong());
	printf("CHECK \t: %d\n", x * y);

	return EXIT_SUCCESS;
}

bitset<bitsetSize> karatsuba(bitset<bitsetSize> x, bitset<bitsetSize> y) {
//	if one bitset is zero one number is zero so the product is zero
	if (binLength(x) == 0 || binLength(y) == 0) {
		return bitset<bitsetSize>();
	}
//	determine max binary length of x and y. k is the amount of bits to store the max length
	int k = log2(max(binLength(x), binLength(y)));

#ifdef DEBUG
	printf(
			"--------------------------------------------- KARATSUBAAAAA!!! ---------------------------------------------\n");
	printf("x \t= %s\ny \t= %s\n", x.to_string().c_str(),
			y.to_string().c_str());
	printf("k \t= \t%d\n", k);
#endif

	if (k == 0 /*|| k== -2147483648 */) {
#ifdef DEBUG
		printf("will return: %d\n", gridMul(x, y));
#endif
		return gridMul(x, y);
	}
//	B = 2^2^(k-1)
	int B = 1 << (1 << (k - 1));
	int log2B = log2(B);
//	x = x0 + x1*B
	bitset<bitsetSize> x0 = copyBits(x, 0, log2B - 1);
	bitset<bitsetSize> x1 = copyBits(x, log2B, bitsetSize - 1);
//	y = y0 + y1*B
	bitset<bitsetSize> y0 = copyBits(y, 0, log2B - 1);
	bitset<bitsetSize> y1 = copyBits(y, log2B, bitsetSize - 1);

#ifdef DEBUG
	printf("B \t= \t%d\n", B);
	printf("log2B \t= \t%d\n", log2B);
	printf("x \t= %s\n", x.to_string().c_str());
	printf("x0 \t= %s\n", x0.to_string().c_str());
	printf("x1 \t= %s\n", x1.to_string().c_str());
	printf("y \t= %s\n", y.to_string().c_str());
	printf("y0 \t= %s\n", y0.to_string().c_str());
	printf("y1 \t= %s\n", y1.to_string().c_str());
#endif

	bitset<bitsetSize> x0y0 = karatsuba(x0, y0);
	bitset<bitsetSize> x1y1 = karatsuba(x1, y1);
	bitset<bitsetSize> x0x1y0y1 = karatsuba(binSub(x0, x1), binSub(y0, y1));

#ifdef DEBUG
	printf("x0y0 \t= %s\n", x0y0.to_string().c_str());
	printf("x1y1 \t= %s\n", x1y1.to_string().c_str());
	printf("x0x1y0y1= %s\n", x0x1y0y1.to_string().c_str());
	printf("y \t= %s\n", y.to_string().c_str());
	printf("y0 \t= %s\n", y0.to_string().c_str());
	printf("y1 \t= %s\n", y1.to_string().c_str());

	//  x0y0 + (x0y0 + x1y1 - (x0-x1)*(y0y1))*B + x1y1*B*B
	//	x0y0+x1y1
	bitset<bitsetSize> result = binAdd(x0y0, x1y1);
	printf("x0y0+x1y1 \t= %s\n", result.to_string().c_str());
	//	result - x0x1y0y1
	result = binSub(result, x0x1y0y1);
	printf("result-x0x1y0y1\t= %s\n", result.to_string().c_str());
	//	result * B
	result <<= log2B;
	printf("result*B \t= %s\n", result.to_string().c_str());
	//	x0y0 + result
	result = binAdd(x0y0, result);
	printf("x0y0+result \t= %s\n", result.to_string().c_str());
	//	result + x1y1*B*B
	result = binAdd(result, (x1y1 << (log2(B * B))));
	printf("result+x1y1*B*B\t= %s\n", result.to_string().c_str());
#endif

#ifndef DEBUG
//  x0y0 + (x0y0 + x1y1 - (x0-x1)*(y0y1))*B + x1y1*B*B
	bitset<bitsetSize> result = binAdd(x0y0, x1y1);
	result = binSub(result, x0x1y0y1);
	result <<= log2B;
	result = binAdd(x0y0, result);
	result = binAdd(result, (x1y1 << (log2(B * B))));
#endif
	return result;
}

inline int gridMul(bitset<bitsetSize> x, bitset<bitsetSize> y) {
	int z = 0;
	for (int i = 0; i < bitsetSize; ++i) {
		if (x[i] != 0)
			for (int j = 0; j < bitsetSize; ++j) {
//				(1 << (i+j) = 1 * 2^(i+j)
				z += y[j] * (1 << (i + j));
			}
	}
	return z;
}

inline int binLength(bitset<bitsetSize> n) {
	for (int i = bitsetSize - 1; i >= 0; --i) {
		if (n[i] == 1)
			return i + 1;
	}
	return 0;
}

inline bitset<bitsetSize> copyBits(bitset<bitsetSize> set, int start, int end) {
	bitset<bitsetSize> retSet;
	int j = 0;
	for (int i = start; i <= end; ++i) {
		retSet[j++] = set[i];
	}
	return retSet;
}

inline bitset<bitsetSize> copyBitsTo(bitset<bitsetSize> source, int start,
		int end, bitset<bitsetSize> target, int pos) {
	for (int i = start; i <= end; ++i) {
		target[pos++] = source[i];
	}
	return target;
}

inline bitset<bitsetSize> binAdd(bitset<bitsetSize> a, bitset<bitsetSize> b) {
	bitset<bitsetSize> result;
	bool carry = 0;
	for (int i = 0; i < bitsetSize; ++i) {
		result[i] = (bool) a[i] ^ (bool) b[i] ^ (bool) carry;
		carry = ((bool) b[i] & (bool) carry) | ((bool) a[i] & (bool) b[i])
				| (bool) (a[i] & (bool) carry);
	}
	return result;
}

inline bitset<bitsetSize> binSub(bitset<bitsetSize> a, bitset<bitsetSize> b) {
	bitset<bitsetSize> n(1);
//	complement b and add 1. Then add a
	return binAdd(a, binAdd(~b, n));
}
