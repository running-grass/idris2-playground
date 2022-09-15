##### Options which a user might set before building go here #####

# Where to install idris2 binaries and libraries (must be an absolute path)
PREFIX ?= $(HOME)/.idris2

IDRIS := idris2

INSTALLDIR = `${IDRIS} --libdir`/lib
SUPPORTDIR = `${IDRIS} --libdir`/support/refc

# For Windows targets. Set to 1 to support Windows 7.
OLD_WIN ?= 0

##################################################################

RANLIB ?= ranlib
AR ?= ar

# IDRIS2_LIB_DIR = $shell()

CFLAGS := -Wall $(CFLAGS) -I$(SUPPORTDIR)
LDFLAGS := $(LDFLAGS)

MACHINE := $(shell $(CC) -dumpmachine)
ifneq (,$(findstring cygwin, $(MACHINE)))
	OS := windows
	SHLIB_SUFFIX := .dll
else ifneq (,$(findstring mingw, $(MACHINE)))
	OS := windows
	SHLIB_SUFFIX := .dll
else ifneq (,$(findstring windows, $(MACHINE)))
	OS := windows
	SHLIB_SUFFIX := .dll
else ifneq (,$(findstring darwin, $(MACHINE)))
	OS := darwin
	SHLIB_SUFFIX := .dylib
else ifneq (, $(findstring bsd, $(MACHINE)))
	OS := bsd
	SHLIB_SUFFIX := .so
else
	OS := linux
	SHLIB_SUFFIX := .so
endif

ifneq ($(OS),windows)
	CFLAGS += -fPIC
else ifneq (, $(findstring NT-6.1,$(shell uname)))
	OLD_WIN = 1
endif

ifeq ($(OS),bsd)
	MAKE := gmake
else
	MAKE := make
endif


BUILD_LIB_DIR := "../build/exec/image_app"

TARGET = libidrisjpeg

LDFLAGS += -lturbojpeg -L$(SUPPORTDIR) -lidris2_refc

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