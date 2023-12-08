#!/bin/sh
find . -name '*.ddp' | entr -cc ./fuehre-aus.sh
