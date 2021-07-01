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

InputData input(string &line1, string &line2, bool extend) {
  string str;
  InputData data;

  data.g = new Permutation(line1, extend);
  data.h = new Permutation(line2, extend);
  if (data.g->size() != data.h->size()) {
    throw invalid_argument("Size of genomes differ");
  }

  return data;
}

void output(ostream &os, int dist, double time) {
  os << "Dist: " << dist;
  os.precision(5);
  os << fixed;
  os << ", Wall Time: " << time << "s" << endl;
}
