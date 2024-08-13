#!/bin/sh
Ausgabe=$(./fuehre-aus.sh)

erwartet1="Das Gesamtvolumen beträgt 14538"
Ergebnis1=$(echo "$Ausgabe" | head -1)
if [ "$Ergebnis1" != "$erwartet1" ]; then
    echo "Test fehlgeschlagen (Teil 1)."
    echo "Erwartet: »$erwartet1«"
    echo "Ergebnis: »$Ergebnis1«"
    exit 1
fi

# erwartet2="Das kleinste Volumen ist 484"
# Ergebnis2=$(echo "$Ausgabe" | tail -1)
# if [ "$Ergebnis2" != "$erwartet2" ]; then
#     echo "Test fehlgeschlagen (Teil 2)."
#     echo "Erwartet: »$erwartet2«"
#     echo "Ergebnis: »$Ergebnis2«"
#     exit 2
# fi

echo "Test erfolgreich." 1>&2
