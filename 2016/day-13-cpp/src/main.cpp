#include "./aoc.cpp"
#include <iostream>

using namespace std;

int main(int argc, char *argv[]) {
    if (argc != 2) {
        cout << "Usage: " << argv[0] << " <puzzle-input>" << endl;
        return 1;
    }

    auto favorite_number = atoi(argv[1]);

    cout << "Part 1: " << part1(31, 39, favorite_number) << endl;
}
