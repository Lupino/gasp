{{={= =}=}}
ARDUINO_CLI ?= arduino-cli
BOARD_TAG ?= arduino:avr:pro
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
