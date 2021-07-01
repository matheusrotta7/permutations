#include <algorithm>
#include <vector>

#include "misc/greedyBP.hpp"
#include "misc/permutation.hpp"
#include "quickcheck/quickcheck/quickcheck.hh"
using namespace quickcheck;

void generate(size_t n, Permutation &perm) {
  string sg, sh;

  vector<int> v1(n+2), v2(n+2);
  for (size_t i = 0; i < v1.size(); i++) {
    v1[i] = i;
    v2[i] = i;
  }
  random_shuffle(v1.begin() + 1, v1.end() - 1);

  for (size_t i = 0; i < v1.size(); i++) {
    sg.append(to_string(v1[i]));
    sg.append(" ");
    sh.append(to_string(v2[i]));
    sh.append(" ");
  }

  Permutation g(sg, false);
  Permutation h(sh, false);
  perm.renumber(g, h);
}

class TestBoundsRev : public Property<Permutation> {
  bool holdsFor(const Permutation &perm) {

    int breaks_count = 0;
    for (size_t b = 1; b <= perm.size() - 1; b++) {
      if (perm.strong_breakpoint(b))
        breaks_count++;
    }

    int dist = reversal_distance_estimation(perm);

    bool sucess = true;
    sucess = sucess && dist >= (breaks_count / 2);
    /* sucess = sucess && dist <= breaks_count; */
    if (!sucess) {
      cout << "pi: " << perm << endl;
      cout << "dist: " << dist << endl;
      cout << "b(pi):" << breaks_count << endl;
    } else {
      cout << ".";
      cout.flush();
    }
    return sucess;
  }
};

class TestBoundsTrans : public Property<Permutation> {
  bool holdsFor(const Permutation &perm) {

    int breaks_count = 0;
    for (size_t b = 1; b <= perm.size() - 1; b++) {
      if (perm.breakpoint(b))
        breaks_count++;
    }

    int dist = transposition_distance_estimation(perm);

    bool sucess = true;
    sucess = sucess && dist >= (breaks_count / 3);
    sucess = sucess && dist <= breaks_count;
    if (!sucess) {
      cout << "pi: " << perm << endl;
      cout << "dist: " << dist << endl;
      cout << "b(pi):" << breaks_count << endl;
    } else {
      cout << ".";
      cout.flush();
    }
    return sucess;
  }
};

class TestBoundsRevTrans : public Property<Permutation> {
  bool holdsFor(const Permutation &perm) {

    int breaks_count = 0;
    for (size_t b = 1; b <= perm.size() - 1; b++) {
      if (perm.strong_breakpoint(b))
        breaks_count++;
    }

    int dist = reversal_and_transposition_distance_estimation(perm);

    bool sucess = true;
    sucess = sucess && dist >= (breaks_count / 3);
    sucess = sucess && dist <= breaks_count;
    if (!sucess) {
      cout << "pi: " << perm << endl;
      cout << "dist: " << dist << endl;
      cout << "b(pi):" << breaks_count << endl;
    } else {
      cout << ".";
      cout.flush();
    }
    return sucess;
  }
};

int main() {
  // set seed
  srand(time(nullptr));

  check<TestBoundsRev>("lower bound hold for reversal");
  check<TestBoundsTrans>("upper and lower bounds hold for transposition");
  check<TestBoundsRevTrans>(
      "upper and lower bounds hold for reversal and transposition");
  return 0;
}
