all: main

main: main.cpp dragon.cpp dragon.pl
	swipl-ld -o main main.cpp dragon.cpp dragon.pl