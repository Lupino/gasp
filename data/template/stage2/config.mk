{{={= =}=}}
ARDUINO_CLI ?= arduino-cli
BOARD_TAG ?= arduino:avr:pro
UPLOAD_FLAGS ?= -P usbasp
BUILD_PATH ?= $(CURDIR)/arduino_build

{=# app =}
PROJECT_NAME ?= {= name =}
{=/ app =}

PROJECT_NAME ?= $(notdir $(patsubst %/build/stage2,%,$(CURDIR)))

TARGET_INO = stage2.ino

ifeq ($(findstring rp2040:rp2040, $(BOARD_TAG)),rp2040:rp2040)
TARGET_APP = $(PROJECT_NAME).uf2
else
TARGET_APP = $(PROJECT_NAME).hex
endif


ARDUINO_DATA = $(HOME)/Library/Arduino15
ARDUINO_LIB_PATH = $(HOME)/Documents/Arduino/libraries
