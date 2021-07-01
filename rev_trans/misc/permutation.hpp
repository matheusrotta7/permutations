#pragma once
#include <vector>
#include <memory>
using namespace std;

typedef int Gene;
typedef int IR;

class Permutation {
protected:
  unique_ptr<vector<Gene>> genes;
  unique_ptr<vector<Gene>> positions;
  Gene op_max;
  void record_positions();

public:
  Permutation(string str_g, bool extend);
  Permutation(){};
  /* Copy constructor */
  Permutation(const Permutation &pi);
  /* Renumber first permutation so that the second can be represented by iota */
  void renumber(const Permutation &s, const Permutation &h);
  /* Verify if the permutation is sorted */
  bool is_iota() const;
  /* Size of permutation */
  size_t size() const { return genes->size(); }
  /* Check in gene is the begin of a breakpoint */
  bool breakpoint(int) const;
  bool strong_breakpoint(int) const;
  /* Move to end of strip */
  int end_of_strip(int);
  int end_of_strong_strip(int);
  /* Only read access to the elements. */
  Gene operator[](int i) const { return (*genes)[i - 1]; }
  /* Get positions of a given label (read only). */
  int &pos(Gene label) const { return (*positions)[label]; }
  /* Reversal operation */
  void reversal(Gene, Gene);
  /* Transposition operation */
  void transposition(Gene, Gene, Gene);
  /* to print */
  void serialize(ostream &) const;
};

ostream &operator<<(ostream &, const Permutation &);
