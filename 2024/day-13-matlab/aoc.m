1; % Octave script file, see https://docs.octave.org/latest/Script-Files.html

pkg load symbolic

function nums = extract_numbers(str)
    [a,b] = regexp(str, '\d+(\.\d+)?');
    nums = zeros(length(a),1);
    for k = 1:length(a)
        nums(k) = str2double(str(a(k):b(k)));
    end
end

function res = parse(data)
    sections = strsplit(strtrim(data), "\n\n");
    for i = 1:length(sections)
        res(i,:) = extract_numbers(sections{i});
    end
end

function [q, rem] = divmod(a, b)
    q = floor(a / b);
    rem = mod(a, b);
end

function res = min_cost(machine)
    [ax, ay, bx, by, px, py] = num2cell(machine){:};

    [i, i_rem] = divmod(px * by - bx * py, ax * by - bx * ay);
    [j, j_rem] = divmod(px - ax * i, bx);

    if i_rem || j_rem
        res = 0;
    else
        res = sym(3 * i + j);
    end
end

function res = required_tokens(machines)
    res = 0;
    for i = 1:size(machines,1)
        res += min_cost(machines(i,:));
    end
end

function res = increase_prices(machines_matrix)
    machines_matrix(:,5:6) += 10000000000000;
    res = machines_matrix;
end

% --- TESTS --------------------------------------------------------------------

function test_parse()
    assert(parse(
        "Button A: X+94, Y+34\n\
         Button B: X+22, Y+67\n\
         Prize: X=8400, Y=5400\n\n\
         Button A: X+26, Y+66\n\
         Button B: X+67, Y+21\n\
         Prize: X=12748, Y=12176\n"),
        [94, 34, 22, 67,  8400,  5400;
         26, 66, 67, 21, 12748, 12176]);
end

function test_min_cost()
    assert(double(min_cost([94, 34, 22, 67, 8400, 5400])), 280)
    assert(double(min_cost([26, 66, 67, 21, 12748, 12176])), 0)
    assert(double(min_cost([17, 86, 84, 37, 7870, 6450])), 200)
    assert(double(min_cost([69, 23, 27, 71, 18641, 10279])), 0)
end

function run_tests()
    test_parse();
    test_min_cost();
end

% --- MAIN ---------------------------------------------------------------------

if nargin == 0
    printf("Usage: octave %s (-t|--test|<input file>)\n", program_name());
    exit(1);
elseif strcmp(argv(){1}, "--test") || strcmp(argv(){1}, "-t")
    run_tests();
    fprintf("All tests passed.\n");
else
    machines = parse(fileread(argv(){1}));

    p1 = required_tokens(machines);
    p2 = required_tokens(increase_prices(machines));

    fprintf('%s\n', char(p1));
    fprintf('%s\n', char(p2));
end
