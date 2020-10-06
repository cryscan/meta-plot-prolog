#include "dragon.h"
#include <SWI-cpp.h>
#include <iostream>
#include <memory>
#include <string>

static std::unique_ptr<PlEngine> engine = nullptr;
static std::unique_ptr<PlTerm> situation = nullptr;

static std::unique_ptr<PlQuery> plan = nullptr;
static std::unique_ptr<PlTerm> process = nullptr;

void init_engine(char *name) {
  engine = std::make_unique<PlEngine>(name);
  process = std::make_unique<PlTerm>();
}

bool init_situation() {
  try {
    PlTermv av(1);
    PlQuery query("s0", av);
    if (query.next_solution()) {
      situation = std::make_unique<PlTerm>(av[0]);
      return true;
    }
  } catch (PlException &ex) {
    std::cerr << (char *)ex << std::endl;
  }
  return false;
}

bool get_home(const char *ch, char *result) {
  try {
    PlTermv home(2);
    home[0] = PlTerm(ch);

    PlTermv fact(1);
    fact[0] = PlCompound("home", home);

    PlQuery query("fact", fact);
    if (query.next_solution()) {
      strcpy(result, (char *)home[1]);
      return true;
    }
  } catch (PlException &ex) {
    std::cerr << (char *)ex << std::endl;
  }
  return false;
}

bool get_level(const char *ch, int &result) {
  try {
    PlTermv level(2);
    level[0] = PlTerm(ch);

    PlTermv fact(1);
    fact[0] = PlCompound("level", level);

    PlQuery query("fact", fact);
    if (query.next_solution()) {
      result = level[1];
      return true;
    }
  } catch (PlException &ex) {
    std::cerr << (char *)ex << std::endl;
  }
  return false;
}

bool get_location(const char *ch, char *result) {
  try {
    PlTermv at(2);
    at[0] = PlTerm(ch);

    PlTermv holds(2);
    holds[0] = PlCompound("at", at);
    holds[1] = *situation;

    PlQuery query("holds", holds);
    if (query.next_solution()) {
      strcpy(result, (char *)at[1]);
      return true;
    }
  } catch (PlException &ex) {
    std::cerr << (char *)ex << std::endl;
  }
  return false;
}

bool parse_bool(const char *str, bool &result) {
  if (strcmp(str, "true") == 0)
    result = true;
  else if (strcmp(str, "false") == 0)
    result = false;
  else
    return false;
  return true;
}

bool get_affection(const char *ch1, const char *ch2, bool &result) {
  try {
    PlTermv affection(2);

    std::string pair = std::string(ch1) + "-" + std::string(ch2);
    affection[0] = PlCompound(pair.c_str());

    PlTermv holds(2);
    holds[0] = PlCompound("affection", affection);
    holds[1] = *situation;

    PlQuery query("holds", holds);
    if (query.next_solution()) {
      return parse_bool(affection[1], result);
    }
  } catch (PlException &ex) {
    std::cerr << (char *)ex << std::endl;
  }
  return false;
}

bool get_hatred(const char *ch1, const char *ch2, bool &result) {
  try {
    PlTermv hatred(2);

    std::string pair = std::string(ch1) + "-" + std::string(ch2);
    hatred[0] = PlCompound(pair.c_str());

    PlTermv holds(2);
    holds[0] = PlCompound("hatred", hatred);
    holds[1] = *situation;

    PlQuery query("holds", holds);
    if (query.next_solution()) {
      return parse_bool(hatred[1], result);
    }
  } catch (PlException &ex) {
    std::cerr << (char *)ex << std::endl;
  }
  return false;
}

bool get_kidnaped(const char *ch, bool &result) {
  try {
    PlTermv kidnaped(2);
    kidnaped[0] = PlTerm(ch);

    PlTermv holds(2);
    holds[0] = PlCompound("kidnaped", kidnaped);
    holds[1] = *situation;

    PlQuery query("holds", holds);
    if (query.next_solution()) {
      return parse_bool(kidnaped[1], result);
    }
  } catch (PlException &ex) {
    std::cerr << (char *)ex << std::endl;
  }
  return false;
}

bool get_married(const char *ch, bool &result) {
  try {
    PlTermv married(2);
    married[0] = PlTerm(ch);

    PlTermv holds(2);
    holds[0] = PlCompound("married", married);
    holds[1] = *situation;

    PlQuery query("holds", holds);
    if (query.next_solution()) {
      return parse_bool(married[1], result);
    }
  } catch (PlException &ex) {
    std::cerr << (char *)ex << std::endl;
  }
  return false;
}

bool get_dead(const char *ch, bool &result) {
  try {
    PlTermv dead(2);
    dead[0] = PlTerm(ch);

    PlTermv holds(2);
    holds[0] = PlCompound("dead", dead);
    holds[1] = *situation;

    PlQuery query("holds", holds);
    if (query.next_solution()) {
      return parse_bool(dead[1], result);
    }
  } catch (PlException &ex) {
    std::cerr << (char *)ex << std::endl;
  }
  return false;
}

void parse_action(const PlTerm &action, Action &result) {
  result = {0};
  strcpy(result.name, (char *)action.name());
  if (action.arity() > 0)
    strcpy(result.a1, (char *)action[1]);
  if (action.arity() > 1)
    strcpy(result.a2, (char *)action[2]);
  if (action.arity() > 2)
    strcpy(result.a3, (char *)action[3]);
}

void get_actions(Action *result, int &count) {
  try {
    PlTerm action;

    PlTermv poss(2);
    poss[0] = action;
    poss[1] = *situation;

    PlQuery query("poss", poss);
    count = 0;
    while (query.next_solution()) {
      Action &dest = result[count];
      parse_action(action, dest);
      ++count;
    }
  } catch (PlException &ex) {
    std::cerr << (char *)ex << std::endl;
  }
}

bool execute_action(const Action *actions, int count) {
  std::string str = "[";
  for (int i = 0; i < count; ++i) {
    auto &action = actions[i];
    str += std::string(action.name) + "(";
    if (strlen(action.a1) > 0)
      str += std::string(action.a1);
    if (strlen(action.a2) > 0)
      str += ", " + std::string(action.a2);
    if (strlen(action.a3) > 0)
      str += ", " + std::string(action.a3);

    str += ")";
    if (i != count - 1)
      str += ", ";
  }
  str += "]";
  // std::cout << str << std::endl;

  try {
    PlCompound action_list(str.c_str());

    PlTermv execute(3);
    execute[0] = *situation;
    execute[1] = action_list;

    PlQuery query("execute_process", execute);
    if (query.next_solution()) {
      // std::cerr << (char *)execute[2] << std::endl;
      situation = std::make_unique<PlTerm>(execute[2]);
      // std::cerr << (char *)*situation << std::endl;
      return true;
    }
  } catch (PlException &ex) {
    std::cerr << (char *)ex << std::endl;
  }
  return false;
}

void init_plan(const char *beat) {
  try {
    PlTerm sit;

    PlTermv _plan(4);
    _plan[0] = PlCompound(beat);
    _plan[1] = *situation;
    _plan[2] = sit;
    _plan[3] = *process;

    plan = std::make_unique<PlQuery>("plan", _plan);
  } catch (PlException &ex) {
    std::cerr << (char *)ex << std::endl;
  }
}

bool get_plan_actions(Action *result, int &count) {
  if (!plan)
    return false;

  try {
    if (plan->next_solution()) {
      PlTail list(*process);
      PlTerm action;

      count = 0;
      while (list.next(action)) {
        Action &dest = result[count];
        parse_action(action, dest);
        ++count;
      }

      return true;
    }
  } catch (PlException &ex) {
    std::cerr << (char *)ex << std::endl;
  }
  return false;
}