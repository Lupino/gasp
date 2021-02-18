#!/usr/bin/env bash

cp ../../data/template/app/* template/app

patch template/app/app.ino template/app/app.ino.patch

sed -i 's/EEPROM\.get/EEPROM_get/g' template/app/app.ino
sed -i 's/EEPROM\.put/EEPROM_put/g' template/app/app.ino
sed -i 's/SERIAL\./SERIAL_/g' template/app/app.ino
sed -i '/FC(F(/s/)))/)/g' template/app/app.ino
sed -i '/FC(F(/s/)),/,/g' template/app/app.ino
sed -i 's/FC(F(//g' template/app/app.ino
