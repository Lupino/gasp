{{={= =}=}}
ARDUINO_CLI ?= arduino-cli
BOARD_TAG ?= arduino:avr:pro
BUILD_PATH ?= $(CURDIR)/arduino_build

{=# app =}
KEY = {= key =}
PROJECT_NAME = {= name =}
{=/ app =}

ifeq ($(BOARD_TAG),rp2040:rp2040:rpipico)
TARGET_APP = $(PROJECT_NAME).uf2
else
TARGET_APP = $(PROJECT_NAME).hex
endif
