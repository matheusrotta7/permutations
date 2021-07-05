#pragma once
#include "permutation.hpp"
#include <iostream>

template<class T>
void print_vec(ostream &os, vector<T> vec) {
	for (auto a : vec) {
		os << a << " ";
	}
}

vector<string>* read_lines(istream &is);
Permutation *input(string &line1, bool extend);
void output(ostream &os, int dist, double time);
