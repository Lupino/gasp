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

init do
#define GL_SERIAL Serial
#define DEBUG_SERIAL Serial
#define SEND_DELAY_MS attr_delay
done

setup do
    GL_SERIAL.begin(115200);
    while (!GL_SERIAL) {;}
done

init do
#define DHTPIN 9     // what digital pin we're connected to
// Uncomment whatever type you're using!
#define DHTTYPE DHT11   // DHT 11
// #define DHTTYPE DHT22   // DHT 22  (AM2302), AM2321
// #define DHTTYPE DHT21   // DHT 21 (AM2301)

// https://github.com/adafruit/DHT-sensor-library.git
#include <DHT.h>

DHT dht(DHTPIN, DHTTYPE);
done

setup do
    dht.begin();
done

func read_temp do
    // Reading temperature or humidity takes about 250 milliseconds!
    // Sensor readings may also be up to 2 seconds 'old' (its a very slow sensor)
    metric_humidity = dht.readHumidity();
    metric_temperature = dht.readTemperature();
done

attr delay {
  type: "unsigned long",
  default: 300,
  min: 60,
  max: 86400,
  scale: 1000
}

metric temperature {
    type: "float",
    max_threshold: 100,
    min_threshold: 1,
    max: 100,
    min: 0,
    threshold: 1,
    prec: 2
}

metric humidity {
    type: "float",
    max_threshold: 100,
    min_threshold: 1,
    max: 100,
    min: 0,
    threshold: 1,
    prec: 2
}

every read_temp 6000

attr relay_state {
  type: "int",
  max: 1,
  min: 0
}

gpio relay {
    pin: "13",
    link: relay_state,
    state: LOW,
    reverse: true
}

rule metric_temperature > 30 do open_gpio_relay else close_gpio_relay
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
