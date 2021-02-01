<p align=center>
  A programming language that understands what a iot device app is.
</p>
<br>

------

Gasp (**G**ivelink **A**pplication **Sp**ecification Language) is an extensible [DSL](https://en.wikipedia.org/wiki/Domain-specific_language) (domain-specific language) for building modern iot device apps with less code.

Concepts such as *app*, *attr*, *metirc*, *init*, *loop*, *setup*, etc. are baked into the language, bringing a new level of expressiveness and allowing you to get more work done with fewer lines of code.

NOTE: Gasp is still in alpha, meaning it has bugs, and many critical features are still missing and it is still changing a lot!

```js
// dbt.wasp:
app DhtApp {
  key: "product_key",
  token: "device_token"
}

init {=code
#define GL_SERIAL Serial
#define DEBUG_SERIAL Serial
#define SEND_DELAY_MS send_delay_ms
code=}

setup {=code
    GL_SERIAL.begin(115200);
    while (!GL_SERIAL) {;}
code=}

init {=code
#define DHTPIN 9     // what digital pin we're connected to
// Uncomment whatever type you're using!
#define DHTTYPE DHT11   // DHT 11
// #define DHTTYPE DHT22   // DHT 22  (AM2302), AM2321
// #define DHTTYPE DHT21   // DHT 21 (AM2301)

// https://github.com/adafruit/DHT-sensor-library.git
#include <DHT.h>

DHT dht(DHTPIN, DHTTYPE);
code=}

setup {=code
    dht.begin();
code=}

func read_temp {=code
    // Reading temperature or humidity takes about 250 milliseconds!
    // Sensor readings may also be up to 2 seconds 'old' (its a very slow sensor)
    humidity = dht.readHumidity();
    temperature = dht.readTemperature();
    return RET_SUCC;
code=}

attr delay {
  var: send_delay_ms,
  type: "unsigned long",
  default: 300,
  min: 60,
  max: 86400,
  scale: 1000
}

metric temperature {
    var: temperature,
    type: "float",
    max_threshold: 100,
    min_threshold: 5,
    max: 100,
    min: 0,
    threshold: 1,
    width: 8,
    prec: 6
}

metric humidity {
    var: humidity,
    type: "float",
    max_threshold: 100,
    min_threshold: 5,
    max: 100,
    min: 0,
    threshold: 1,
    width: 8,
    prec: 6
}

monitor dht {
  fn: read_temp,
  delay_ms: 6000
}

init {=code
    int mock_sensor_data = 0;
code=}

func read_sensor_data {=code
    sprintf(retval, FC(F("{\"sensor_data\": %d}")), mock_sensor_data);
    return RET_SUCC;
code=}

flag read_sensor_data {
    retval: true
}

telemetry {
    fn: read_sensor_data
}

command get_sensor_data {
    fn: read_sensor_data,
    error: "read sensor data error"
}

func some_poll {=code
    return RET_SUCC;
code=}

loop {=code
    some_poll();
code=}
```

Source files (`.wasp`, `.ino`, `.c`, `.h`, ...) are compiled (transpiled) by `gaspc` (Gasp compiler) into the iot technology stack of your choice (e.g. Arduino + sensor + ...).

:arrow_forward: Check out [DhtApp example](examples/dht) for complete code example. :arrow_backward:

Why is Gasp awesome:
- **Quick start**: Due to its expressiveness, you can create and deploy a production-ready iot device app from scratch with very few lines of concise, consistent, declarative code.
- **Flexible**: When you need more control than Gasp offers, you can write code in existing technologies such as c/h/... and combine it with Gasp code!
- **No lock-in**: If Gasp becomes too limiting for you, simply eject and continue with the generated source code, which is human-readable.

For more information about Gasp, check [**docs**](https://www.jianshu.com/p/98fa9bb363cc).

# This repository

This is the main repo of the Gasp universe, containing core code (mostly `gaspc` - Gasp compiler) and the supporting materials.
