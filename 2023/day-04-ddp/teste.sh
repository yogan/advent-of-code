#!/bin/bash
set -euo pipefail
Ausgabe=$(./fuehre-aus.sh)

Ergebnis1=$(echo "${Ausgabe}" | head -1)
Ergebnis2=$(echo "${Ausgabe}" | tail -1)

erwartet1="Teil 1: 15268"
erwartet2="Teil 2: 6283755"

if [ "${Ergebnis1}" != "${erwartet1}" ]; then
    echo "Test fehlgeschlagen (Teil 1)."
    echo "Erwartet: »${erwartet1}«"
    echo "Ergebnis: »${Ergebnis1}«"
    exit 1
fi

if [ "${Ergebnis2}" != "${erwartet2}" ]; then
    echo "Test fehlgeschlagen (Teil 2)."
    echo "Erwartet: »${erwartet2}«"
    echo "Ergebnis: »${Ergebnis2}«"
    exit 2
fi

echo "Test erfolgreich." 1>&2