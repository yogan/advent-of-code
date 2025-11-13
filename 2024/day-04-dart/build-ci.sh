#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")" || exit 1

dart pub upgrade
dart compile exe bin/aoc.dart
