# Advent of Code 2022 Day 15

Part 2 needs some brute force and can take a long time. Final solution is quite
fast by only checking the borders of the beacon areas. But still, I learned two
important Python tricks to deal with this:

1. Run with [PyPy](https://www.pypy.org/) instead of CPython to reduce runtime
   by a factor of almost 10:

    ```shell
    $ time python3 day15.py
    Executed in   19.12 secs    fish           external
    usr time   19.11 secs   72.00 micros   19.11 secs
    sys time    0.01 secs  295.00 micros    0.01 secs

    $ time pypy3 day15.py
    Executed in    2.51 secs    fish           external
    usr time    2.48 secs  121.00 micros    2.48 secs
    sys time    0.03 secs  499.00 micros    0.03 secs
    ```

    Installation: `sudo apt install pypy3`

2. Use [tqdm](https://github.com/tqdm/tqdm) to show nice progress bars with
   no effort (just wrap iterators). This helps to estimate if a solution will
   finish in a reasonable amount of time.

   Installation:
   - `pypy3 -mpip install tqdm` (PyPy)
   - `pip install tqdm` (CPython)
