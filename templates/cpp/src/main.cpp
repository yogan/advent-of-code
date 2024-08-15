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
    auto boxes = read_lines(argv[1]);

    cout << part1(boxes) << endl;
}
