#ifndef _DRAGON_H_
#define _DRAGON_H_

#include "export.h"

struct Action {
  char name[16];
  char a1[16];
  char a2[16];
  char a3[16];
};

extern "C" {
EXPORTED void init_engine(char *name);

EXPORTED bool init_situation();

EXPORTED bool get_home(const char *ch, char *result);
EXPORTED bool get_level(const char *ch, int &result);

EXPORTED bool get_location(const char *ch, char *result);
EXPORTED bool get_affection(const char *ch1, const char *ch2, bool &result);
EXPORTED bool get_hatred(const char *ch1, const char *ch2, bool &result);
EXPORTED bool get_kidnaped(const char *ch, bool &result);
EXPORTED bool get_married(const char *ch, bool &result);
EXPORTED bool get_dead(const char *ch, bool &result);

EXPORTED void get_actions(Action *result, int &count);
EXPORTED bool execute_action(const Action *actions, int count);

EXPORTED void init_plan(const char *beat);
EXPORTED bool get_plan_actions(Action *result, int &count);
}

#endif