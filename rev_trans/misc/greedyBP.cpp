#include "greedyBP.hpp"
#include <cassert>
#include <cmath>

/* Try to apply a full deterministic reversal that do not add a strong
 * breakpoint, return false if not possible */
bool apply_0sbp_fulldet_rev(Permutation &pi) {
  int i, j;

  for (size_t l = 2; l < pi.size(); l++) {
    j = max(int(l), pi[l] + 1);
    i = min(int(l), pi[l] + 1);
    if (i != j && pi[pi[l] + 1] == int(l) - 1 && pi.strong_breakpoint(i - 1) &&
        pi.strong_breakpoint(j)) {
      pi.reversal(i, j);
      return true;
    }
  }
  return false;
}

/* Try to apply a deterministic reversal that remove a strong breakpoint,
 * return false if not possible */
bool apply_1sbp_det_rev(Permutation &pi) {
  int i, j;

  for (size_t l = 2; l < pi.size(); l++) {
    j = max(int(l), pi[l] + 1);
    i = min(int(l), pi[l] + 1);
    if (pi.strong_breakpoint(i - 1) && pi.strong_breakpoint(j) &&
        ((abs(pi[j] - pi[i - 1]) == 1) || (abs(pi[j + 1] - pi[i]) == 1))) {
      pi.reversal(i, j);
      return true;
    }
  }
  return false;
}

/* Try to apply a deterministic reversal that remove two strong breakpoints,
 * return false if not possible */
bool apply_2sbp_det_rev(Permutation &pi) {
  int i, j;

  for (size_t l = 2; l < pi.size(); l++) {
    j = max(int(l), pi[l] + 1);
    i = min(int(l), pi[l] + 1);
    if (pi.strong_breakpoint(i - 1) && pi.strong_breakpoint(j) &&
        (abs(pi[j] - pi[i - 1]) == 1) && (abs(pi[j + 1] - pi[i]) == 1)) {
      pi.reversal(i, j);
      return true;
    }
  }
  return false;
}

bool gen_bp(const Permutation &pi, int i, bool strong) {
  if (strong) {
    return pi.strong_breakpoint(i);
  } else {
    return pi.breakpoint(i);
  }
}

bool apply_nxbp_det_trans_aux(Permutation &pi, int bp, bool strong, int i,
                              int j, int k) {
  int old_bp = gen_bp(pi, i - 1, strong) + gen_bp(pi, j - 1, strong) +
               gen_bp(pi, k - 1, strong);
  int new_bp;
  if (strong) {
    new_bp = (abs(pi[j] - pi[i - 1]) != 1) + (abs(pi[i] - pi[k - 1]) != 1) +
             (abs(pi[k] - pi[j - 1]) != 1);
  } else {
    new_bp = (pi[j] - pi[i - 1] != 1) + (pi[i] - pi[k - 1] != 1) +
             (pi[k] - pi[j - 1] != 1);
  }
  if (old_bp - new_bp == bp) {
    pi.transposition(i, j, k);
    return true;
  } else {
    return false;
  }
}

bool apply_nxbp_det_trans(Permutation &pi, int bp, bool strong) {
  size_t j;

  for (size_t i = 2; i < pi.size() - 1; i++) {
    j = pi.pos(i - 1);
    if (i < j) {
      for (size_t k = j + 1; k <= pi.size(); k++) {
        if (apply_nxbp_det_trans_aux(pi, bp, strong, i, j, k))
          return true;
      }
    }
    for (size_t j = i + 1; j < pi.size(); j++) {
      size_t k = pi[i] + j - i + 2;
      if (j < k && k <= pi.size()) {
        if (apply_nxbp_det_trans_aux(pi, bp, strong, i, j, k))
          return true;
      }
    }
  }
  return false;
}

/* Try to apply a deterministic transposition that remove a breakpoint,
 * return false if not possible */
bool apply_1bp_det_trans(Permutation &pi) {
  int i, j, k;
  int e = pi.end_of_strip(1);
  i = e + 1;
  j = pi.pos(e);
  k = pi.end_of_strip(j) + 1;
  pi.transposition(i, j, k);
  return true;
}

/* Try to apply a deterministic transposition that remove two breakpoints,
 * return false if not possible */
bool apply_2bp_det_trans(Permutation &pi) {
  return apply_nxbp_det_trans(pi, 2, false);
}

/* Try to apply a deterministic transposition that remove three breakpoints,
 * return false if not possible */
bool apply_3bp_det_trans(Permutation &pi) {
  return apply_nxbp_det_trans(pi, 3, false);
}

/* Try to apply a deterministic transposition that remove a strong breakpoint,
 * return false if not possible */
bool apply_1sbp_det_trans(Permutation &pi) {
  int i, j, k;
  int e = pi.end_of_strong_strip(1);
  i = e + 1;
  j = pi.pos(e);
  k = pi.end_of_strong_strip(j) + 1;
  pi.transposition(i, j, k);
  return true;
}

/* Try to apply a deterministic transposition that remove two strong
 * breakpoints, return false if not possible */
bool apply_2sbp_det_trans(Permutation &pi) {
  return apply_nxbp_det_trans(pi, 2, true);
}

/* Try to apply a deterministic transposition that remove three strong
 * breakpoints, return false if not possible */
bool apply_3sbp_trans(Permutation &pi) {
  return apply_nxbp_det_trans(pi, 3, true);
}

int transposition_distance_estimation(Permutation pi) {
  int dist = 0;
  bool ok;

  while (!pi.is_iota()) {
    ok = apply_3bp_det_trans(pi) || apply_2bp_det_trans(pi) ||
         apply_1bp_det_trans(pi);
    assert(ok);
    dist++;
  }

  return dist;
}

int reversal_distance_estimation(Permutation pi) {
  int dist = 0;
  bool ok;

  while (!pi.is_iota()) {
    ok = apply_2sbp_det_rev(pi) || apply_1sbp_det_rev(pi) ||
         apply_0sbp_fulldet_rev(pi);
    if (!ok) {
      int e = pi.end_of_strong_strip(1);
      pi.reversal(e + 1, pi.pos(e));
    }
    dist++;
  }

  return dist;
}

int reversal_and_transposition_distance_estimation(Permutation pi) {
  int dist = 0;
  bool ok;

  while (!pi.is_iota()) {
    ok = apply_3sbp_trans(pi) || apply_2sbp_det_rev(pi) ||
         apply_2sbp_det_trans(pi) || apply_1sbp_det_rev(pi) ||
         apply_1sbp_det_trans(pi);
    assert(ok);
    dist++;
  }

  return dist;
}
