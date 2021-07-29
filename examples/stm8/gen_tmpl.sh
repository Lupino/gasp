#!/usr/bin/env bash

cp -av ../../data/template/* template/

sed -i 's/EEPROM\.get/EEPROM_get/g' template/combined.gasp
sed -i 's/EEPROM\.put/EEPROM_put/g' template/combined.gasp
sed -i 's/EEPROM\.read/EEPROM_read/g' template/combined.gasp
sed -i 's/EEPROM\.write/EEPROM_write/g' template/combined.gasp
sed -i 's/EEPROM\.commit/EEPROM_commit/g' template/combined.gasp
sed -i 's/EEPROM\.begin/EEPROM_begin/g' template/combined.gasp
sed -i 's/SERIAL\./SERIAL_/g' template/combined.gasp
sed -i '/FC(F(/s/)))/)/g' template/combined.gasp
sed -i '/FC(F(/s/)),/,/g' template/combined.gasp
sed -i 's/FC(F(//g' template/combined.gasp
sed -i 's/\.begin/_begin/g' template/combined.gasp
sed -i 's/\.available/_available/g' template/combined.gasp
sed -i 's/\.read/_read/g' template/combined.gasp
sed -i 's/\.write/_write/g' template/combined.gasp

sed -i 's/BOARD_TAG ?= .*/BOARD_TAG ?= sduino:stm8:stm8sdisco/g' template/config.mk
sed -i 's/\.with_bootloader//g' template/Makefile

echo 'require "./dtostrf.gasp"' >> template/combined.gasp
