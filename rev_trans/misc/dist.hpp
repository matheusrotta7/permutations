#pragma once

#include "permutation.hpp"

class GreedyBPAlg {
public:
    virtual int estimate_distance(Permutation pi) = 0;
};
