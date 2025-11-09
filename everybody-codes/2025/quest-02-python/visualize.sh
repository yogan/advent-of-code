#!/bin/sh
# I only have PIL for python3, not for pypy3 (via the python3-pil:amd64 Ubuntu
# package), so let's use python3 to create the images. But not for the "normal"
# calculation run, as it is magnitudes slower than pypy3.
python3 ec.py --visualize
