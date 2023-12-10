# Advent of Code 2023 Day 10

To find the enclosed areas between the pipe path, I had to increase the whole
grid by a factor of two, so that the "running between the pipes" works as
expected.

For the final result, the grid is scaled down again (throwing away all entries
with odd coordinates).

## Sample Input Visualization

![Sample (enlarged by 2)](viz/sample_big.png)

![Sample (final result)](viz/sample.png)

## Real Input Visualization

![Real input (enlarged by 2, top left)](viz/input_big_topleft.png)

![Real input (enlarged by 2, center)](viz/input_big_center.png)

![Real input (final result)](viz/input.png)
