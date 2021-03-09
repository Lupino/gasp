TARGET=blink/build button/build dht/build ds18b20/build l76x/build led/build reset/build stm8/build

all: $(TARGET)

%/build: %
	cd $< && gasp compile

clean:
	rm -r $(TARGET)
