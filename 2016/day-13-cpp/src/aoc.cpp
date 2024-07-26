#include <bitset>
#include <iostream>
#include <queue>
#include <string>
#include <tuple>
#include <vector>

using namespace std;

const int MAX_COORD = 50;

int count_bits(int n) {
    return bitset<32>(n).count();
}

int count_bits(bitset<MAX_COORD> bitsets[MAX_COORD]) {
    int count = 0;
    for (int i = 0; i < MAX_COORD; i++) {
        count += bitsets[i].count();
    }
    return count;
}

bool is_wall(int x, int y, int favorite_number) {
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
vector<string> printable_maze(int max_x, int max_y, int favorite_number) {
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

pair<int, int> flood_fill(int target_x, int target_y, int favorite_number) {
    vector<pair<int, int>> directions = {make_pair(0, -1), make_pair(0, 1),
                                         make_pair(-1, 0), make_pair(1, 0)};

    bitset<MAX_COORD> visited[MAX_COORD];

    queue<tuple<int, int, int>> queue;
    queue.push(make_tuple(1, 1, 0));

    int min_steps = -1;
    int visited_count = -1;

    while (!queue.empty()) {
        auto [x, y, steps] = queue.front();
        queue.pop();

        if (steps >= MAX_COORD && visited_count == -1) {
            visited_count = count_bits(visited);
        }

        if (x == target_x && y == target_y && min_steps == -1) {
            min_steps = steps;
        }

        if (min_steps != -1 && visited_count != -1) {
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
