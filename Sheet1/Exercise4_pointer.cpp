/*
 * Exercise4_v1.cpp
 *
 *  Created on: 26 Apr 2015
 *      Author: seriously
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <vector>
#include <sstream>
#include <iterator>
#include <string>
#include <boost/dynamic_bitset.hpp>

using namespace std;

// uncomment this and recompile for very verbose output
//#define DEBUG

inline int gridMul(vector<bool> x, vector<bool> y);
/**
 * changes both vectors to the size of the bigger one
 */
/*
 * resizes and adds two binary numbers
 */
inline boost::dynamic_bitset<> binAdd(boost::dynamic_bitset<> *a,
		boost::dynamic_bitset<> *b);
inline void resize(boost::dynamic_bitset<> *a, boost::dynamic_bitset<> *b);
/**
 * TODO: currently only terminates for positive ints
 */
vector<bool> intToVec(int n);
string bitsetToString(boost::dynamic_bitset<> vec);

int main(int argc, char *argv[]) {

	if (argc != 3) {
		printf("bad number of arguments!\n");
		printf("usage: %s x y\n", argv[0]);
		return EXIT_FAILURE;
	}

//	parse input
	stringstream ss;
	ss << argv[1] << ' ' << argv[2];
	int x, y;
	if (!(ss >> x)) {
		printf("first argument is not a valid number!\n");
		return EXIT_FAILURE;
	}
	if (!(ss >> y)) {
		printf("second argument is not a valid number!\n");
		return EXIT_FAILURE;
	}

	//	convert to binary representation
	boost::dynamic_bitset<> x_bit(floor(log2(x)) + 1, x);
	boost::dynamic_bitset<> y_bit(floor(log2(y)) + 1, y);

	printf("x_bit\t= %s\n", bitsetToString(x_bit).c_str());
	printf("y_bit\t= %s\n", bitsetToString(y_bit).c_str());

	boost::dynamic_bitset<> z_bit = binAdd(&x_bit, &y_bit);
	x_bit <<= 1;
	printf("SERIOUS STUFF HAPPENED\n");
	printf("x_bit\t= %s\n", bitsetToString(x_bit).c_str());
	printf("y_bit\t= %s\n", bitsetToString(y_bit).c_str());
	printf("z_bit\t= %s\n", bitsetToString(z_bit).c_str());
//	boost::dynamic_bitset<> z_bit = bitsetToString(x_vec, y_vec);
//	printf("z_bit\t= %s\n", bitsetToString(z_vec).c_str());
//
	printf("x \t= %ld\n", x_bit.to_ulong());
	printf("y \t= %ld\n", y_bit.to_ulong());
	printf("z \t= %ld\n", z_bit.to_ulong());
}

inline int gridMul(vector<bool> *x, vector<bool> *y) {
	int z = 0;
//	for (unsigned int i = 0; i < (*x).size(); ++i) {
//		if (x[i] != 0)
//			for (unsigned int j = 0; j < (*y).size(); ++j) {
////				(1 << (i+j) = 1 * 2^(i+j)
//				z += y[j] * (1 << (i + j));
//			}
//	}
	return z;
}

inline boost::dynamic_bitset<> binAdd(boost::dynamic_bitset<> *a,
		boost::dynamic_bitset<> *b) {
	resize(a, b);
	boost::dynamic_bitset<> result;
	bool carry = 0;
	for(unsigned int i = 0; i < a->size(); ++i){
		result.push_back((*a)[i]^(*b)[i] ^ carry);
		carry = ((*b)[i] &  carry) | ( (*a)[i] &  (*b)[i]) |  ((*a)[i] &  carry);
	}
	if(carry)
		result.push_back(1);
	return result;
}


//	for (int i = 0; i < (*a).size(); ++i) {
//		result.push_back((bool)(a[i]^b[i]^carry));
//		carry = ((bool) b[i] & (bool) carry) | ((bool) a[i] & (bool) b[i])
//				| (bool) (a[i] & (bool) carry);
//	}

inline void resize(boost::dynamic_bitset<> *a, boost::dynamic_bitset<> *b) {
	int sizediff = (*a).size() - (*b).size();
//	a > b
	if (sizediff > 0) {
		for (; sizediff > 0; sizediff--) {
			(*b).push_back(0);
		}
	} else {
		for (; sizediff < 0; sizediff++) {
			(*a).push_back(0);
		}
	}
}

vector<bool> intToVec(int n) {
	vector<bool> vec;
	if (n == 0) {
		vec.push_back(0);
	}
	while (n) {
		if (n & 1)
			vec.push_back(1);
		else
			vec.push_back(0);
		n >>= 1;
	}
	return vec;
}

string bitsetToString(boost::dynamic_bitset<> bitset) {
	ostringstream oss;
	for (unsigned int i = bitset.size() - 1; i > 0; --i) {
		oss << bitset[i] << " ";
	}
	oss << bitset[0];

	return oss.str();

}
