{{={= =}=}}
ARDUINO_CLI ?= arduino-cli
BOARD_TAG ?= sduino:stm8:stm8sdisco
BUILD_PATH ?= $(CURDIR)/arduino_build

{=# app =}
KEY = {= key =}
{=/ app =}

PROJECT_NAME ?= $(notdir $(patsubst %/build,%,$(CURDIR)))

TARGET_INO = app/app.ino

ifeq ($(BOARD_TAG),rp2040:rp2040:rpipico)
TARGET_APP = $(PROJECT_NAME).uf2
else
TARGET_APP = $(PROJECT_NAME).hex
endif

define get_arduino_dir
    $(shell $(ARDUINO_CLI) config dump | grep $1 | awk -F ':' '{print $$2}')
endef

ARDUINO_DATA = $(call get_arduino_dir, data)
ARDUINO_LIB_PATH = $(call get_arduino_dir, user)/libraries
