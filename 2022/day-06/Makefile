BINARY = day06
SOURCES = day06.c

CC = gcc
CFLAGS = -Wall -Wextra -O2

all: $(BINARY)

$(BINARY): $(SOURCES)
	$(CC) $(CFLAGS) -o $(BINARY) $(SOURCES)

run: $(BINARY)
	./$(BINARY)

clean:
	rm -f $(BINARY)
