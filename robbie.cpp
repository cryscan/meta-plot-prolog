#include "export.h"
#include <SWI-cpp.h>
#include <iostream>

static PlEngine engine("librobbie.so");

static PlTerm situation;

extern "C" {
EXPORTED void initialize() {
  PlTermv av(1);
  PlQuery query("s0", av);
  situation = av[0];
}

EXPORTED void search_robbie() {
  PlTermv av(2);
  PlQuery query("a_star", av);

  if (query.next_solution()) {
    PlTail sit = av[0], process = av[1];
    PlTerm action;

    while (process.next(action)) {
      std::cout << (char *)action << std::endl;
    }
  }
}
}
