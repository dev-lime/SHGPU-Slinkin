CFLAGS = -Wall -Wextra -std=c99 -Wno-unused-parameter
TARGET = main
SOURCES = main.c squnitA.c squnitL.c

all: $(TARGET)

$(TARGET): $(SOURCES)
	gcc $(CFLAGS) -o $(TARGET) $(SOURCES)

clean:
	rm -f $(TARGET)

test: $(TARGET)
	./$(TARGET)

.PHONY: all clean test
