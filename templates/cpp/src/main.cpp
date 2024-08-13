#include "./aoc.cpp"
#include <fstream>
#include <iostream>
#include <vector>

using namespace std;

vector<tuple<int, int, int>> read_lines(string filename) {
    vector<tuple<int, int, int>> boxes;
    ifstream file(filename);
    string line;
    while (getline(file, line)) {
        boxes.push_back(parse_line(line));
    }
    return boxes;
}

int main(int argc, char *argv[]) {
    auto filename = argc > 1 ? argv[1] : "input.txt";
    auto boxes = read_lines(filename);

    cout << part1(boxes) << endl;
}
