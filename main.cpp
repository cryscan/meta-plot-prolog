#include "dragon.h"
#include <iostream>

void print_actions(const Action *actions, int count) {
  for (int i = 0; i < count; ++i) {
    auto &action = actions[i];
    std::cout << action.name << " " << action.a1 << " " << action.a2 << " "
              << action.a3 << std::endl;
  }
}

int main(int argc, char **argv) {
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
  init_plan("prelude-1");
  if (!get_plan_actions(actions, count))
    std::cout << "shit!" << std::endl;
  print_actions(actions, count);
}