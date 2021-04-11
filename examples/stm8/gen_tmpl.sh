#!/usr/bin/env bash

cp ../../data/template/app/* template/app

patch template/app/app.ino template/app/app.ino.patch

sed -i 's/EEPROM\.get/EEPROM_get/g' template/app/app.ino
sed -i 's/EEPROM\.put/EEPROM_put/g' template/app/app.ino
sed -i 's/EEPROM\.read/EEPROM_read/g' template/app/app.ino
sed -i 's/EEPROM\.write/EEPROM_write/g' template/app/app.ino
sed -i 's/EEPROM\.commit/EEPROM_commit/g' template/app/app.ino
sed -i 's/EEPROM\.begin/EEPROM_begin/g' template/app/app.ino
sed -i 's/SERIAL\./SERIAL_/g' template/app/app.ino
sed -i '/FC(F(/s/)))/)/g' template/app/app.ino
sed -i '/FC(F(/s/)),/,/g' template/app/app.ino
sed -i 's/FC(F(//g' template/app/app.ino
sed -i 's/\.begin/_begin/g' template/app/app.ino
sed -i 's/\.available/_available/g' template/app/app.ino
sed -i 's/\.read/_read/g' template/app/app.ino
sed -i 's/\.write/_write/g' template/app/app.ino
