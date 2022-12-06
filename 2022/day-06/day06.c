#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

const char *FILENAME = "day06.in";
const size_t LINE_SIZE = 4096;
const size_t WINDOW_SIZE = 4;

bool has_four_unique_chars(const char *str, size_t offset)
{
    if (str == NULL || strlen(str) < offset + WINDOW_SIZE)
    {
        return false;
    }

    for (size_t i = offset; i < offset + WINDOW_SIZE; i++)
    {
        char c = str[i];
        for (size_t j = offset; j < offset + WINDOW_SIZE; j++)
        {
            if (i != j && c == str[j])
            {
                return false;
            }
        }
    }

    return true;
}

size_t find_marker(char *line)
{
    for (size_t i = 0; line[i] != '\0' && line[i] != '\n'; i++)
    {
        if (has_four_unique_chars(line, i))
        {
            return i + WINDOW_SIZE;
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

void test(char *input, size_t expected)
{
    size_t marker = find_marker(input);
    if (marker != expected)
    {
        printf("Error (input: \"%s\"): expected marker %zu, got %zu\n",
               input, expected, marker);
        return;
    }
    printf("Test passed (input: \"%s\")\n", input);
}

void run_tests()
{
    test("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 7);
    test("bvwbjplbgvbhsrlpgdmjqwftvncz", 5);
    test("nppdvjthqldpwncqszvftbrmjlhg", 6);
    test("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 10);
    test("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 11);
}

int main()
{
    run_tests();
    printf("\n");

    char line[LINE_SIZE];
    read_file(line);
    size_t marker = find_marker(line);
    printf("Marker: %ld\n", marker);

    return 0;
}
