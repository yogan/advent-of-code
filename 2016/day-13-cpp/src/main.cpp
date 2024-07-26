#include "./aoc.cpp"
#include <iostream>

using namespace std;

int main(const int argc, const char *argv[]) {
    if (argc != 2) {
        cout << "Usage: " << argv[0] << " <puzzle-input>" << endl;
        return 1;
    }

    auto favorite_number = atoi(argv[1]);
    auto [min_steps, visited] = flood_fill(31, 39, favorite_number);

    cout << "Part 1: " << min_steps << endl;
    cout << "Part 2: " << visited << endl;
}
