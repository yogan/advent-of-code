1; % Octave script file, see https://docs.octave.org/latest/Script-Files.html

function res = parse(data)
    lines = strsplit(strtrim(data), "\n");
    res = {};
    for i = 1:length(lines)
        res{end+1} = str2double(strsplit(lines{i}, "x"));
    end
end

function res = volume(box)
    res = prod(box);
end

function res = part1(boxes)
    res = sum(cellfun(@volume, boxes));
end

function res = surface_area(box)
    [l, w, h] = deal(box(1), box(2), box(3));
    res = 2 * l * w + 2 * w * h + 2 * h * l;
end

function res = part2(boxes)
    res = sum(cellfun(@surface_area, boxes));
end

% --- TESTS --------------------------------------------------------------------

function test_parse()
    assert(parse("1x2x3\n4x5x666\n"), {[1, 2, 3], [4, 5, 666]});
end

function test_part1(sample, expected)
    assert(part1(sample), expected);
end

function test_part2(sample, expected)
    assert(part2(sample), expected);
end

function run_tests()
    sample = {[1, 2, 3], [1, 1, 1]};
    test_parse();
    test_part1(sample, 6 + 1);
    test_part2(sample, (2 + 2 + 3 + 3 + 6 + 6) + (6 * 1));
end

% --- MAIN ---------------------------------------------------------------------

if nargin == 0
    printf("Usage: octave %s (-t|--test|<input file>)\n", program_name());
    exit(1);
elseif strcmp(argv(){1}, "--test") || strcmp(argv(){1}, "-t")
    run_tests();
    fprintf("All tests passed.\n");
else
    boxes = parse(fileread(argv(){1}));
    fprintf('%d\n', part1(boxes));
    fprintf('%d\n', part2(boxes));
end
