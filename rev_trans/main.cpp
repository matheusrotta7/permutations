#include "misc/greedyBP.hpp"
#include "misc/io.hpp"
#include "misc/permutation.hpp"
#include "misc/timer.hpp"
#include <experimental/filesystem>
#include <fstream>
#include <getopt.h>
#include <iostream>
#include <sstream>
namespace fs = experimental::filesystem;
using namespace std;

#define N_POS_ARGS 1

struct Args {
  string input_file;
  string output_folder;
  int iterations = 1;
  bool extend = false;
  bool duplicate = false;
  string alg;
};

void help(char *name) {
  cout
      << "usage: Calculate distances for pairs of permutations." << endl
      << "\t" << name << " ALG [OPTIONS]" << endl
      << endl
      << "positional arguments:" << endl
      << "\tALG                     the algorithm to use (trans, rev, revtrans)"
      << endl
      << endl
      << "optional arguments:" << endl
      << "\t-h, --help              show this help message and exit" << endl
      << "\t-i, --input INPUT       input file (if not provided stdin is used)"
      << endl
      << "\t-o, --output OUTPUT     outpute folder (if not provided stdout is "
         "used)"
      << endl
      << "\t-e, --extend            whether to extend the genomes before "
         "apply the algorithm"
      << endl;

  exit(EXIT_SUCCESS);
}

void get_args(Args &args, int argc, char *argv[]) {
  extern char *optarg;
  extern int optind;
  int n_pos_args = 0;

  struct option longopts[] = {{"input", 1, NULL, 'i'},
                              {"output", 1, NULL, 'o'},
                              {"extend", 0, NULL, 'e'},
                              {"help", 0, NULL, 'h'}};

  char op;
  while ((op = getopt_long(argc, argv, "i:o:he", longopts, NULL)) != -1) {
    switch (op) {
    case 'i':
      args.input_file = optarg;
      break;
    case 'o':
      args.output_folder = optarg;
      break;
    case 'e':
      args.extend = true;
      break;
    default:
      help(argv[0]);
    }
  }
  for (int i = optind; i < argc; i++) {
    args.alg = argv[i];
    n_pos_args++;
  }

  if (n_pos_args != N_POS_ARGS) {
    help(argv[0]);
  }
}

int main(int argc, char *argv[]) {
  Args args;
  ifstream is;
  unique_ptr<vector<string>> input_lines;

  get_args(args, argc, argv);

  // set seed
  srand(1);

  if (args.input_file != "") {
    is.open(args.input_file);
    input_lines.reset(read_lines(is));
    is.close();
  } else {
    input_lines.reset(read_lines(cin));
  }

  try {
    if (input_lines->size() % 4 == 1) {
      throw invalid_argument("Number of lines is not multiple of 4.");
    }
#pragma omp parallel for
    for (size_t i = 0; i < input_lines->size(); i += 4) {
      Timer timer;
      ofstream os;
      int dist;

      InputData data =
          input((*input_lines)[i], (*input_lines)[i + 1], args.extend);
      auto g = unique_ptr<Permutation>(data.g);
      auto h = unique_ptr<Permutation>(data.h);
      auto pi = unique_ptr<Permutation>(new Permutation());
      pi->renumber(*g, *h);

      // run algorithm
      if (args.alg == "rev") {
        dist = reversal_distance_estimation(*pi);
      } else if (args.alg == "trans") {
        dist = transposition_distance_estimation(*pi);
      } else if (args.alg == "revtrans") {
        dist = reversal_and_transposition_distance_estimation(*pi);
      } else {
      }

      if (args.output_folder != "") {
        os.close();
        os.open(args.output_folder + '/' +
                fs::path(args.input_file).filename().c_str() +
                string(5 - to_string(i / 4).size(), '0') + to_string(i / 4));
      }

      if (args.output_folder != "") {
        output(os, dist, timer.elapsed_time());
      } else {
        output(cout, dist, timer.elapsed_time());
      }
    }

  } catch (const invalid_argument &e) {
    cerr << "Something went wrong!!!" << endl;
    cerr << e.what() << endl;
  }

  return 0;
}
