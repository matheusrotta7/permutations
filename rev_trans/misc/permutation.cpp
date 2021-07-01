#include "permutation.hpp"
#include <algorithm>
#include <cassert>
#include <sstream>
#include <vector>

Permutation::Permutation(string str_g, bool extend) {
  string token;
  op_max = -1;
  genes.reset(new vector<Gene>());

  /* Read each element of the string. */
  if (extend)
    genes->push_back(0);
  stringstream ssg(str_g);
  while (getline(ssg, token, ' ')) {
    Gene a = stoi(token);
    genes->push_back(a);
    if (abs(genes->back()) > op_max)
      op_max = abs(genes->back());
  }
  if (extend) {
    genes->push_back(op_max + 1);
    op_max++;
  }

  record_positions();
}

Permutation::Permutation(const Permutation &pi) {
  genes = unique_ptr<vector<Gene>>(new vector<Gene>(*pi.genes));
  positions = unique_ptr<vector<Gene>>(new vector<Gene>(*pi.positions));
  op_max = pi.op_max;
}

void Permutation::renumber(const Permutation &g, const Permutation &h) {
  genes.reset(new vector<Gene>(g.size()));
  op_max = h.size();

  /* Build vector mapping old to new labels */
  vector<Gene> labels(h[h.size()] + 1);
  for (size_t i = 0; i < h.size(); i++) {
    labels[abs(h[i + 1])] = (h[i + 1] >= 0) ? i : -i;
  }

  /* Add genes changing labels */
  (*genes)[0] = labels[abs(g[1])];
  for (size_t i = 2; i <= g.size() - 1; i++) {
    (*genes)[i - 1] = labels[abs(g[i])];
  }
  (*genes)[g.size() - 1] = labels[g[g.size()]];

  record_positions();
}

void Permutation::record_positions() {
  /* Record list of positions for each label. */
  positions.reset(new vector<Gene>(op_max + 1));
  for (size_t i = 0; i < genes->size(); ++i) {
    (*positions)[abs((*genes)[i])] = i + 1;
  }
}

bool Permutation::is_iota() const {
  bool ok = true;

  for (size_t i = 0; i < size() - 1 && ok; i++) {
    if ((*genes)[i + 1] - (*genes)[i] != 1)
      ok = false;
  }

  return ok;
}

bool Permutation::breakpoint(int i) const {
  return (*genes)[i] - (*genes)[i - 1] != 1;
}

bool Permutation::strong_breakpoint(int i) const {
  return abs((*genes)[i] - (*genes)[i - 1]) != 1;
}

int Permutation::end_of_strip(int i) {
    while(i < int(size()) && !breakpoint(i)) i++;
    return i;
}

int Permutation::end_of_strong_strip(int i) {
    while(i < int(size()) && !strong_breakpoint(i)) i++;
    return i;
}

void Permutation::reversal(Gene i, Gene j) {

  assert(2 <= i);
  assert(i < j);
  assert(j < int(size()));

  for (int k = 0; k < (j - i + 1) / 2; k++) {
    swap((*genes)[i + k - 1], (*genes)[j - k - 1]);
  }

  record_positions();
}

void Permutation::transposition(Gene i, Gene j, Gene k) {

  assert(2 <= i);
  assert(i < j);
  assert(j < k);
  assert(k <= int(size()));

  vector<Gene> aux1;
  vector<IR> aux2;
  for (int t = i - 1; t <= j - 2; t++) {
    aux1.push_back((*genes)[t]);
  }
  int t = i - 1;
  for (int r = j - 1; r <= k - 2; r++) {
    (*genes)[t] = (*genes)[r];
    t++;
  }
  for (size_t r = 0; r < aux1.size(); r++) {
    (*genes)[t] = aux1[r];
    t++;
  }

  record_positions();
}

void Permutation::serialize(ostream &os) const {
  os << "(";
  for (size_t i = 1; i <= size() - 1; ++i) {
    os << (*this)[i] << " ";
  }
  os << (*this)[size()] << ")";
}

ostream &operator<<(ostream &os, const Permutation &g) {
  g.serialize(os);
  return os;
}
