/*
 * Exercise4_v1.cpp
 *
 *  Created on: 26 Apr 2015
 *      Author: seriously
 */

#include <stdio.h>
#include <stdlib.h>
#include <vector>
#include <sstream>
#include <iterator>
#include <string>

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
inline vector<bool> binAdd(vector<bool> a, vector<bool> b);
inline void resize(vector<bool> *a, vector<bool> *b);
/**
 * TODO: currently only terminates for positive ints
 */
vector<bool> intToVec(int n);
string vecToString(vector<bool> vec);

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
	vector<bool> x_vec = intToVec(x);
	vector<bool> y_vec = intToVec(y);

	printf("x_vec\t= %s\n", vecToString(x_vec).c_str());
	printf("y_vec\t= %s\n", vecToString(y_vec).c_str());

//	vector<bool> z_vec = gridMul(x_vec, y_vec);
//	printf("z_vec\t= %s\n", vecToString(z_vec).c_str());

	printf("x \t= %d\n", x);
	printf("y \t= %d\n", y);
	int z = gridMul(x_vec, y_vec);
	printf("z \t= %d\n", z);
}

inline int gridMul(vector<bool> *x, vector<bool> *y) {
	int z = 0;
	for (unsigned int i = 0; i < (*x).size(); ++i) {
		if (x[i] != 0)
			for (unsigned int j = 0; j < (*y).size(); ++j) {
//				(1 << (i+j) = 1 * 2^(i+j)
				z += y[j] * (1 << (i + j));
			}
	}
	return z;
}

inline vector<bool> binAdd(vector<bool> *a, vector<bool> *b) {
	resize(a,b);
	vector<bool> result;
	bool carry = 0;
	for (int i = 0; i < (*a).size(); ++i) {
		result.push_back((bool)(a[i]^b[i]^carry));
		carry = ((bool) b[i] & (bool) carry) | ((bool) a[i] & (bool) b[i])
				| (bool) (a[i] & (bool) carry);
	}
	return result;
}

inline void resize(vector<bool> *a, vector<bool> *b) {
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

string vecToString(vector<bool> vec) {
	ostringstream oss;

	if (!vec.empty()) {
		copy(vec.begin(), vec.end() - 1, ostream_iterator<bool>(oss, " "));
		oss << vec.back();
	}
	return oss.str();

}
