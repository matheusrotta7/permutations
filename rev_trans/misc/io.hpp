#pragma once
#include "permutation.hpp"
#include <iostream>

template<class T>
void print_vec(ostream &os, vector<T> vec) {
	for (auto a : vec) {
		os << a << " ";
	}
}

struct InputData {
	Permutation *g;
	Permutation *h;
};

vector<string>* read_lines(istream &is);
InputData input(string &line1, string &line2, bool extend);
void output(ostream &os, int dist, double time);
