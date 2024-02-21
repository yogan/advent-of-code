#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

const char *FILENAME = "day06.in";
const size_t LINE_SIZE = 4096;

bool has_four_unique_chars(const char *str, size_t offset, size_t window_size)
{
    if (str == NULL || strlen(str) < offset + window_size)
    {
        return false;
    }

    for (size_t i = offset; i < offset + window_size; i++)
    {
        char c = str[i];
        for (size_t j = offset; j < offset + window_size; j++)
        {
            if (i != j && c == str[j])
            {
                return false;
            }
        }
    }

    return true;
}

size_t find_marker(char *line, size_t window_size)
{
    for (size_t i = 0; line[i] != '\0' && line[i] != '\n'; i++)
    {
        if (has_four_unique_chars(line, i, window_size))
        {
            return i + window_size;
        }
    }
    printf("No marker found in line: %s", line);
    return -1;
}

void read_file(char *line)
{
    FILE *file = fopen(FILENAME, "r");
    if (file == NULL)
    {
        printf("Error: unable to open file\n");
        exit(1);
    }

    char *result = fgets(line, LINE_SIZE, file);
    if (result == NULL)
    {
        printf("Error: unable to read line\n");
        exit(1);
    }

    fclose(file);
}

void test(char *input, size_t window_size, size_t expected)
{
    size_t marker = find_marker(input, window_size);

    if (marker != expected)
    {
        printf("Error (input: \"%s\", window: \"%zu\"): expected marker %zu, got %zu\n",
               input, window_size, expected, marker);
        return;
    }

    printf("Test passed (input: \"%s\", window: \"%zu\")\n",
           input, window_size);
}

void run_tests()
{
    test("mjqjpqmgbljsphdztnvjfqwrcgsmlb", /*   */ 4, 7);
    test("bvwbjplbgvbhsrlpgdmjqwftvncz", /*     */ 4, 5);
    test("nppdvjthqldpwncqszvftbrmjlhg", /*     */ 4, 6);
    test("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", /**/ 4, 10);
    test("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", /* */ 4, 11);

    test("mjqjpqmgbljsphdztnvjfqwrcgsmlb", /*   */ 14, 19);
    test("bvwbjplbgvbhsrlpgdmjqwftvncz", /*     */ 14, 23);
    test("nppdvjthqldpwncqszvftbrmjlhg", /*     */ 14, 23);
    test("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", /**/ 14, 29);
    test("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", /* */ 14, 26);
}

int main()
{
    run_tests();
    printf("\n");

    char line[LINE_SIZE];
    read_file(line);

    size_t marker1 = find_marker(line, 4);
    printf("Part 1 (start-of-packet marker):  %ld\n", marker1);

    size_t marker2 = find_marker(line, 14);
    printf("Part 2 (start-of-message marker): %ld\n", marker2);

    return 0;
}
