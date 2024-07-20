#include <cassert>
#include <iostream>
#include <string>
#include <tuple>
#include <vector>

using namespace std;

tuple<int, int, int> parse_line(string str) {
    vector<int> nums;
    string part;

    for (auto c : str) {
        if (c == 'x') {
            nums.push_back(stoi(part));
            part = "";
        } else {
            part += c;
        }
    }

    nums.push_back(stoi(part));

    assert(nums.size() == 3);
    return make_tuple(nums[0], nums[1], nums[2]);
}

int volume(int l, int w, int h) { return l * w * h; }

int part1(vector<tuple<int, int, int>> boxes) {
    int result = 0;

    for (auto box : boxes) {
        auto [l, w, h] = box;
        result += volume(l, w, h);
    }

    return result;
}
