all: rp2040

rp2040: compile
	make -C build/stage2 BOARD_TAG=rp2040:rp2040:rpipicow
	cp build/stage2/*.uf2 .

esp32c3:
	make -C build/stage2 BOARD_TAG=esp32:esp32:esp32c3

compile:
	gasp compile

upload:
	make -C build/stage2 BOARD_TAG=esp32:esp32:esp32c3 UPLOAD_FLAGS='--port /dev/cu.usbserial-A907TWMX' upload

clean:
	gasp clean
