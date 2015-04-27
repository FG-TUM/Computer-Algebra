/*
 * Exercise4.cpp
 *
 *  Created on: 26 Apr 2015
 *      Author: seriously
 */

#include <stdio.h>
#include <stdlib.h>
#include <algorithm> // max
#include <vector>
#include <string>
#include <iterator>
#include <sstream>
#include <math.h> //log2, ceil

using namespace std;

int karatsuba(vector<bool> x, vector<bool> y);
int gridMul(vector<bool> x, vector<bool> y);
vector<bool> binSub(vector<bool> x, vector<bool> y);
string vecToString(vector<bool> vec);
vector<bool> convertIntToVec(int x);
//vector<bool> convertStringToVec(string s);

int main(int argc, char **argv) {

	int x = 651;
	int y = 5646;

	vector<bool> x_vec = convertIntToVec(x);
	vector<bool> y_vec = convertIntToVec(y);


	printf("x \t= \t%d\n", x);
	printf("y \t= \t%d\n", y);
	printf("x_vec \t= \t%s\n", vecToString(x_vec).c_str());
	printf("y_vec \t= \t%s\n", vecToString(y_vec).c_str());
	vector<bool> z_vec = binSub(x_vec, y_vec);
	printf("z_vec \t= \t%s\n", vecToString(z_vec).c_str());

}

int karatsuba(vector<bool> x, vector<bool> y) {
//	determine max binary length of x and y. k is the amount of bits to store the max length
	int k = ceil(log2(max(x.size(), y.size())));
	printf("k \t= \t%d\n", k);
	if (k == 0)
		return gridMul(x, y);
//	B = 2^2^(k-1)
	int B = 1 << (1 << (k - 1));
//	printf("B \t= \t%d\n", B);
	int log2B = log2(B);
	vector<bool> x0(x.begin(), x.begin() + log2B);
//	printf("x0 \t= \t%s\n", vecToString(x0).c_str());
	vector<bool> x1(x.begin() + log2B, x.end());
//	printf("x1 \t= \t%s\n", vecToString(x1).c_str());
	vector<bool> y0(y.begin(), y.begin() + log2B);
	vector<bool> y1(y.begin() + log2B, y.end());

	int x0y0 = karatsuba(x0, y0);
	int x1y1 = karatsuba(x1, y1);
	int x0x1y0y1;

	return 0;
}

int gridMul(vector<bool> x, vector<bool> y) {
	int z = 0;
	for (unsigned int i = 0; i < x.size(); ++i) {
		if (x[i] != 0)
			for (unsigned int j = 0; j < y.size(); ++j) {
//				(1 << (i+j) = 1 * 2^(i+j)
				z += y[j] * (1 << (i + j));
			}
	}
	return z;
}

vector<bool> binSub(vector<bool> a, vector<bool> b) {
	printf("START BINSUB\n");
//	pad vectors to same size
	int sizediff = a.size() - b.size();
	if (sizediff > 0) {
		for (int i = 0; i < sizediff; ++i) {
			b.push_back(0);
		}
	}
	if (sizediff < 0) {
		for (int i = 0; i > sizediff; --i) {
			a.push_back(0);
		}
	}

//	complement b
	for(unsigned int i = 0; i < b.size(); i++){
		if(b[i] == 1){
			b[i] = 0;
		} else
			b[i] = 1;
	}

//	bitwise add a and b to c
	vector<bool> c;
	bool carry = 0;
	for(unsigned int i = 0; i < b.size(); i++){
		c.push_back((bool)a[i] ^ (bool)b[i] ^ (bool)carry);
		carry = ((bool)b[i] & (bool)carry) | ((bool)a[i] & (bool)b[i]) | (bool)(a[i] & (bool)carry);
	}

	printf("x_vec \t= \t%s\n", vecToString(a).c_str());
	printf("y_vec \t= \t%s\n", vecToString(b).c_str());

	return c;
}

string vecToString(vector<bool> vec) {
	ostringstream oss;
	if (!vec.empty()) {
		copy(vec.begin(), vec.end() - 1, ostream_iterator<int>(oss, " "));
	}
	oss << vec.back();
	return oss.str();
}

vector<bool> convertIntToVec(int x) {
	vector<bool> ret;
	while (x) {
		if (x & 1)
			ret.push_back(1);
		else
			ret.push_back(0);
		x >>= 1;
	}
	return ret;
}

int convertVecToInt(vector<bool> vec) {
	int
}

//vector<bool> convertStringToVec(string s) {
////	TODO:
//	vector<bool> v;
//	return v;
//}

