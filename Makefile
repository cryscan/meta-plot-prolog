all: main

main: main.cpp dragon.cpp dragon.pl
	swipl-ld -cc-options,-std=c++17 -o main main.cpp dragon.cpp dragon.pl