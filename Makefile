BIN_NAME = haskell-bot

CC = ghc
CFLAGS +=
SRC += Main.hs

.PHONY: all
all: $(BIN_NAME) TAGS

$(BIN_NAME): $(SRC)
	$(CC) $(CFLAGS) $(SRC) -o $@

.PHONY: clean
clean :
	$(RM) *.hi *.o $(BIN_NAME) TAGS

.PHONY: debug
debug: CFLAGS += -g -O0 -DDEBUG
debug: all

TAGS: $(SRC)
	echo ":etags" | ghci -v0 $(addprefix \*,$(SRC))
