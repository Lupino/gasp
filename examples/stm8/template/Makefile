{{={= =}=}}
include config.mk

LIBRARIES_URL +=  {=# imports =}{= url =} {=/ imports =}
LIBRARIES += $(patsubst %.git, $(ARDUINO_LIB_PATH)/%, $(notdir $(LIBRARIES_URL)))

CLI_FLAGS += -b $(BOARD_TAG)
ARDUINO_COMPILE = $(ARDUINO_CLI) compile --build-path $(BUILD_PATH) $(CLI_FLAGS)

all: $(TARGET_APP)

$(ARDUINO_LIB_PATH)/%:
	git clone $(filter %/$(notdir $@).git, $(LIBRARIES_URL)) $@

ifeq ($(BOARD_TAG),rp2040:rp2040:rpipico)
$(TARGET_APP): $(TARGET_INO) $(LIBRARIES)
	$(ARDUINO_COMPILE) $(dir $<)
	cp $(BUILD_PATH)/$(notdir $<).uf2 $@
else
$(TARGET_APP): $(TARGET_INO) $(LIBRARIES)
	$(ARDUINO_COMPILE) $(dir $<)
	cp $(BUILD_PATH)/$(notdir $<).hex $@
endif
