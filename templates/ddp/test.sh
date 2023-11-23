#!/bin/sh
result=$(./fuehre-aus.sh | tail -1)
expected="Das Gesamtvolumen beträgt 14538"

if [ "$result" = "$expected" ]; then
    echo "Test erfolgreich."
    exit 0
else
    echo "Test fehlgeschlagen."
    echo "Erwarted: $expected"
    echo "Erhalten: $result"
    exit 1
fi
