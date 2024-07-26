#include <iostream>
#include <queue>
#include <string>
#include <tuple>
#include <vector>

using namespace std;

int count_bits(int n) {
    int count = 0;
    while (n) {
        count += n & 1;
        n >>= 1;
    }
    return count;
}

bool is_wall(int x, int y, int favorite_number) {
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

int part1(int target_x, int target_y, int favorite_number) {
    vector<pair<int, int>> directions = {make_pair(0, -1), make_pair(0, 1),
                                         make_pair(-1, 0), make_pair(1, 0)};

    vector<int> visited(50, 0); // used as a bitset

    queue<tuple<int, int, int>> queue;
    queue.push(make_tuple(1, 1, 0));

    while (!queue.empty()) {
        auto [x, y, steps] = queue.front();
        queue.pop();

        if (x == target_x && y == target_y) {
            return steps;
        }

        for (auto [dx, dy] : directions) {
            int xx = x + dx;
            int yy = y + dy;

            if (xx < 0 || yy < 0 || is_wall(xx, yy, favorite_number) ||
                visited[xx] & (1 << yy)) {
                continue;
            }

            visited[xx] |= (1 << yy);
            queue.push(make_tuple(xx, yy, steps + 1));
        }
    }

    throw runtime_error("No path found");
}
