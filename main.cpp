#include "dragon.h"
#include <cstdio>
#include <cstring>
#include <iostream>

void print_actions(const Action *actions, int count) {
  for (int i = 0; i < count; ++i) {
    auto &action = actions[i];
    std::cout << action.name << " " << action.a1 << " " << action.a2 << " "
              << action.a3 << std::endl;
  }
}

void test() {
  init_situation();

  char buffer[2048] = {0};

  get_location("princess", buffer);
  std::cout << buffer << std::endl;

  get_home("dragon", buffer);
  std::cout << buffer << std::endl;

  int level;
  get_level("troy", level);
  std::cout << level << std::endl;

  bool affection = false;
  if (!get_affection("felix", "princess", affection))
    std::cout << "shit!" << std::endl;
  std::cout << affection << std::endl;

  bool hatred = false;
  if (!get_hatred("troy", "felix", hatred))
    std::cout << "shit!" << std::endl;
  std::cout << hatred << std::endl;

  bool married = false;
  if (!get_married("princess", married))
    std::cout << "shit!" << std::endl;
  std::cout << married << std::endl;

  bool dead = false;
  if (!get_dead("dragon", dead))
    std::cout << "shit!" << std::endl;
  std::cout << dead << std::endl;

  Action actions[32];
  int count;
  get_actions(actions, count);
  print_actions(actions, count);

  if (!execute_action(actions, 1))
    std::cout << "shit!" << std::endl;

  std::cout << std::endl;
  // init_plan("prelude-1");
  if (!get_plan_actions("prelude-1", actions, count))
    std::cout << "shit!" << std::endl;
  print_actions(actions, count);
}

void listen() {
  std::string line;

  char buf[1024] = {0};
  Beat beats[8];
  Action actions[32];

  while (std::getline(std::cin, line)) {
    char a1[16] = {0};
    char a2[16] = {0};
    char a3[16] = {0};
    char a4[16] = {0};

    if (line == "exit")
      return;

    if (line == "restart")
      init_situation();

    if (sscanf(line.c_str(), "home %s", a1) == 1) {
      get_home(a1, buf);
      std::cout << buf << std::endl;
    }
    if (sscanf(line.c_str(), "level %s", a1) == 1) {
      int result;
      get_level(a1, result);
      std::cout << result << std::endl;
    }

    if (sscanf(line.c_str(), "location %s", a1) == 1) {
      get_location(a1, buf);
      std::cout << buf << std::endl;
    }
    if (sscanf(line.c_str(), "affection %s %s", a1, a2) == 2) {
      bool result;
      get_affection(a1, a2, result);
      std::cout << result << std::endl;
    }
    if (sscanf(line.c_str(), "hatred %s %s", a1, a2) == 2) {
      bool result;
      get_hatred(a1, a2, result);
      std::cout << result << std::endl;
    }
    if (sscanf(line.c_str(), "kidnaped %s", a1) == 1) {
      bool result;
      get_kidnaped(a1, result);
      std::cout << result << std::endl;
    }
    if (sscanf(line.c_str(), "married %s", a1) == 1) {
      bool result;
      get_married(a1, result);
      std::cout << result << std::endl;
    }
    if (sscanf(line.c_str(), "dead %s", a1) == 1) {
      bool result;
      get_dead(a1, result);
      std::cout << result << std::endl;
    }

    if (line == "actions") {
      int count;
      get_actions(actions, count);
      print_actions(actions, count);
    }
    if (sscanf(line.c_str(), "execute %s %s %s %s", a1, a2, a3, a4) > 0) {
      Action action = {0};
      strcpy(action.name, a1);
      strcpy(action.a1, a2);
      strcpy(action.a2, a3);
      strcpy(action.a3, a4);
      if (!execute_action(&action, 1))
        std::cerr << "fail" << std::endl;
    }

    /*if (sscanf(line.c_str(), "plan %s", a1) > 0) {
      init_plan(a1);
    }*/
    if (sscanf(line.c_str(), "predict %s", a1) > 0) {
      int count;
      get_plan_actions(a1, actions, count);
      print_actions(actions, count);
    }

    if (line == "candidates") {
      int count;
      get_candidate_beats(beats, count);

      for (int i = 0; i < count; ++i) {
        char *beat = beats[i].name;
        std::cout << beat << std::endl;
      }
    }

    int phase;
    if (sscanf(line.c_str(), "candidates %d", &phase) > 0) {
      int count;
      get_phase_beats(phase, beats, count);

      for (int i = 0; i < count; ++i) {
        char *beat = beats[i].name;
        std::cout << beat << std::endl;
      }
    }
    if (sscanf(line.c_str(), "goals %d", &phase) > 0) {
      int count;
      get_goal_beats_phase(phase, beats, count);

      for (int i = 0; i < count; ++i) {
        char *beat = beats[i].name;
        std::cout << beat << std::endl;
      }
    }

    std::cout << "###---" << std::endl;
  }
}

int main(int argc, char **argv) {
  char name[32];
  strcpy(name, argv[0]);
  init_engine(name);

  init_situation();
  listen();
}