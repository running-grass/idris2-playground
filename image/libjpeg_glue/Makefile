include ../../config.mk

IDRIS := idris2
INSTALLDIR = `${IDRIS} --libdir`/jpeg/lib

BUILD_LIB_DIR := "../build/exec/image_app"

TARGET = libidrisjpeg

LDFLAGS += -ljpeg -L/usr/local/Cellar/jpeg-turbo/2.1.4/lib

SRCS = $(wildcard *.c)
OBJS = $(SRCS:.c=.o)
DEPS = $(OBJS:.o=.d)


all: $(TARGET)$(SHLIB_SUFFIX)

$(TARGET)$(SHLIB_SUFFIX): $(OBJS)
	$(CC) -shared $(LDFLAGS) -o $@ $^


-include $(DEPS)

%.d: %.c
	@$(CPP) $(CFLAGS) $< -MM -MT $(@:.d=.o) >$@


.PHONY: clean

clean :
	rm -f $(OBJS) $(TARGET)$(SHLIB_SUFFIX)

cleandep: clean
	rm -f $(DEPS)




$(BUILD_LIB_DIR)/$(TARGET)$(SHLIB_SUFFIX): all
	install $(TARGET)$(SHLIB_SUFFIX) $(BUILD_LIB_DIR)

.PHONY: buildcopy
buildcopy: $(BUILD_LIB_DIR)/$(TARGET)$(SHLIB_SUFFIX)

.PHONY: install

install:
	@if ! [ -d $(INSTALLDIR) ]; then mkdir -p $(INSTALLDIR); fi
	install $(TARGET)$(SHLIB_SUFFIX) $(wildcard *.h) $(INSTALLDIR)