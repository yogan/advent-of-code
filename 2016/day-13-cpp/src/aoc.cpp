#include <array>
#include <bitset>
#include <iostream>
#include <queue>
#include <string>
#include <tuple>
#include <vector>

using namespace std;

const int MAX_COORD = 50;
const int UNDEFINED = -1;

const int count_bits(const int n) { return bitset<32>(n).count(); }

const bool is_wall(const int x, const int y, const int favorite_number) {
    if (x < 0 || y < 0) {
        return true;
    }
    int value = x * x + 3 * x + 2 * x * y + y + y * y + favorite_number;
    return count_bits(value) % 2 != 0;
}

/**
 * Just here because this way we can easily test our is_wall function with the
 * sample input in a visual way.
 */
const vector<string> printable_maze(const int max_x, const int max_y,
                                    const int favorite_number) {
    vector<string> maze;

    for (int y = 0; y < max_y; y++) {
        string row;
        for (int x = 0; x < max_x; x++) {
            row += is_wall(x, y, favorite_number) ? "#" : ".";
        }
        maze.push_back(row);
    }

    return maze;
}

const int total_bits(const bitset<MAX_COORD> bitsets[MAX_COORD]) {
    int count = 0;
    for (int i = 0; i < MAX_COORD; i++) {
        count += bitsets[i].count();
    }
    return count;
}

const pair<int, int> flood_fill(const int target_x, const int target_y,
                                const int favorite_number) {
    constexpr array<pair<int, int>, 4> directions = {
        make_pair(0, -1), make_pair(0, 1), make_pair(-1, 0), make_pair(1, 0)};

    bitset<MAX_COORD> visited[MAX_COORD];

    queue<tuple<int, int, int>> queue;
    queue.push(make_tuple(1, 1, 0));

    int min_steps = UNDEFINED;
    int visited_count = UNDEFINED;

    while (!queue.empty()) {
        auto [x, y, steps] = queue.front();
        queue.pop();

        if (steps == MAX_COORD && visited_count == UNDEFINED) {
            visited_count = total_bits(visited);
        }

        if (x == target_x && y == target_y && min_steps == UNDEFINED) {
            min_steps = steps;
        }

        if (min_steps != UNDEFINED && visited_count != UNDEFINED) {
            return make_pair(min_steps, visited_count);
        }

        for (auto [dx, dy] : directions) {
            int xx = x + dx;
            int yy = y + dy;

            if (is_wall(xx, yy, favorite_number) || visited[xx][yy]) {
                continue;
            }

            visited[xx][yy] = true;
            queue.push(make_tuple(xx, yy, steps + 1));
        }
    }

    throw runtime_error("No path found");
}
