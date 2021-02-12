<p align=center>
  A programming language that understands what a iot device app is.
</p>
<br>

------

Gasp (**G**ivelink **A**pplication **Sp**ecification Language) is an extensible [DSL](https://en.wikipedia.org/wiki/Domain-specific_language) (domain-specific language) for building modern iot device apps with less code.

Concepts such as *app*, *attr*, *metirc*, *init*, *loop*, *setup*, etc. are baked into the language, bringing a new level of expressiveness and allowing you to get more work done with fewer lines of code.

NOTE: Gasp is still in alpha, meaning it has bugs, and many critical features are still missing and it is still changing a lot!

```js
// dht.wasp:
app dht {
  key: "product_key",
  token: "device_token"
}


init do
#define GL_SERIAL Serial
#define DEBUG_SERIAL Serial
#define METRIC_DELAY_MS attr_delay
done

setup do
    GL_SERIAL.begin(115200);
    while (!GL_SERIAL) {;}
done

attr delay {
  type: "unsigned long",
  default: 1800,
  min: 60,
  max: 86400,
  scale: 1000
}

metric temperature {
  type: "float",
  max: 100,
  min: 0,
  threshold: 1,
  prec: 2
}

func read_dht do
    metric_temperature += 0.1;
    if (metric_temperature > 100) {
         metric_temperature = 0;
    }
done

every read_dht 6000

attr relay_state {
  type: "int",
  default: 0,
  min: 0,
  max: 1,
  gen_set: false
}

func try_set_attr_relay_state do
    if (attr_relay_mode == 1) {
        return set_attr_relay_state(json, tokens, num_tokens, retval);
    }
    return RET_ERR;
done

command set_relay_state {
    fn: try_set_attr_relay_state,
    error: "only relay_mode is 1 can set this value"
}

attr relay_mode {
  type: "int",
  default: 0,
  min: 0,
  max: 1
}

func try_toggle_gpio_relay do
    if (attr_relay_mode == 1) {
        toggle_gpio_relay();
    }
done

gpio relay_mode LED_BUILTIN -> link relay_mode
gpio relay 12 -> link relay_state
gpio btn0 11 HIGH -> click try_toggle_gpio_relay
gpio btn1 10 HIGH -> click toggle_gpio_relay_mode

attr high_temperature {
  type: "float",
  default: 30,
  min: 0,
  max: 100
}

attr low_temperature {
  type: "float",
  default: 20,
  min: 0,
  max: 100
}
rule metric_temperature < attr_high_temperature && metric_temperature > attr_low_temperature && attr_relay_mode == 1 do open_gpio_relay else close_gpio_relay

init do
bool want_reboot = false;
done

loop do
    if (want_reboot) {
        reset();
    }
done

func reset_system do
    want_reboot = true;
done

command reset_system {
    fn: reset_system
}
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
