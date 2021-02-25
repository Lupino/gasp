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
  key: "1234567890abcdef",   // product_key
  token: "1234567890abcdef", // device_token
  addr: "00000000",
  start_addr: 0,
  ctrl_mode: false
}

GL_SERIAL = Serial
DEBUG_SERIAL = Serial
METRIC_DELAY_MS = attr_delay
// PING_FAILED_CB = noop
// AUTH_DELAY_MS = 1000
// PONG_DELAY_MS = 6000
// PING_DELAY_MS = 300000
// PING_FAILED_CB = noop
// MAX_PING_FAILED = 10
// MAX_GL_PAYLOAD_LENGTH = {= max_gl_len =}
// MAX_BUFFER_LENGTH = {= max_buf_len =}
// MAX_NUM_TOKENS = 10
// MAX_REQUEST_VALUE_LENGTH = {= max_req_len =}
// MAX_TMPL_LENGTH = {= max_tpl_len =}
// METRIC_DELAY_MS = 1800000
// DEBOUNCE_DELAY_MS = 50

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
  gen_set: false,
  keep: false
}

func try_set_attr_relay_state do
    if (attr_relay_mode == 1) {
        return set_attr_relay_state(json, tokens, num_tokens, retval);
    }
    return RET_ERR;
done

command set_relay_state {
    fn: try_set_attr_relay_state,
    error: "only relay_mode is 1 can set this value",
    docs: {
        name: "Edit attribute relay_state",
        command: {
            docs: ["data is between [0, 1]"],
            payload: {"method": "set_relay_state", "data": 0}
        },
        return: {
            docs: ["relay_state is between [0, 1]"],
            payload: {"relay_state": 0}
        },
        error: {
            payload: {"err": "data must between: [0, 1]"}
        }
    }
}

// relay_mode 1 manual mode
//            0 auto mode
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

attr open_delay {
  type: "unsigned long",
  default: 5,
  min: 0,
  max: 3600,
  scale: 1000
}

attr close_delay {
  type: "unsigned long",
  default: 5,
  min: 0,
  max: 3600,
  scale: 1000
}
rule metric_temperature < attr_high_temperature && metric_temperature > attr_low_temperature do later attr_open_delay open_gpio_relay else later attr_close_delay close_gpio_relay on attr_relay_mode == 0

init do
#include <avr/wdt.h>
bool want_reboot = false;
done

setup do
    MCUSR = 0;
    wdt_disable();
    wdt_enable(WDTO_8S);
done

loop do
    wdt_reset();
    if (want_reboot) {
        reset();
    }
done

func reset do
    wdt_disable();
    wdt_enable(WDTO_15MS);
    for (;;) {

    }
done

func reset_system do
    want_reboot = true;
done

command reset_system {
    fn: reset_system
}

init do
bool can_emit_givelink_unauth = false;
done

func emit_givelink_unauth do
    if (can_emit_givelink_unauth) {
        can_emit_givelink_unauth = false;
        givelink_context_set_auth(false);
    }
done

func allow_emit_givelink_unauth do
    can_emit_givelink_unauth = true;
done

gpio auth 9 LOW -> click noop

rule gpio_auth_state == HIGH do later 2000 emit_givelink_unauth else allow_emit_givelink_unauth on givelink_context_authed()
```

- compiled syntax see <doc.md> Template Special.

Source files (`.wasp`, `.ino`, `.c`, `.h`, ...) are compiled (transpiled) by `gaspc` (Gasp compiler) into the iot technology stack of your choice (e.g. Arduino + sensor + ...).

:arrow_forward: Check out [DhtApp example](examples/dht) for complete code example. :arrow_backward:

Why is Gasp awesome:
- **Quick start**: Due to its expressiveness, you can create and deploy a production-ready iot device app from scratch with very few lines of concise, consistent, declarative code.
- **Flexible**: When you need more control than Gasp offers, you can write code in existing technologies such as c/h/... and combine it with Gasp code!
- **No lock-in**: If Gasp becomes too limiting for you, simply eject and continue with the generated source code, which is human-readable.

For more information about Gasp, check [**docs**](https://www.jianshu.com/p/98fa9bb363cc).

# This repository

This is the main repo of the Gasp universe, containing core code (mostly `gaspc` - Gasp compiler) and the supporting materials.
