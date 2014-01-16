SRC      = src
TARGET   = __PROJECT-NAME__.byte
MAIN = $(SRC)/$(TARGET)

.PHONY: compile
compile : $(TARGET)

$(TARGET) : $(SRCS)
	corebuild $(MAIN)

.PHONY: clean
clean :
	rm -f *.byte
	rm -rf _build
