#include "io.hpp"
#include "permutation.hpp"
#include <iostream>
#include <sstream>

vector<string> *read_lines(istream &is) {
  auto input_lines = new vector<string>();

  for (string line; getline(is, line);) {
    if (line.at(0) != '#') {
      input_lines->push_back(line);
    }
  }

  return input_lines;
}

Permutation *input(string &line1, bool extend) {
  Permutation *pi = new Permutation(line1, extend);
  return pi;
}

void output(ostream &os, int dist, double time) {
  os << "Dist: " << dist;
  os.precision(5);
  os << fixed;
  os << ", Wall Time: " << time << "s" << endl;
}
