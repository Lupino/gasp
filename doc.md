# Language Special
## app

```
app appName {
    key: "you_key",
    token: "you_token"
}
```

## Constant

```
name = value
```

## init

```
init do
// you init code
done
```

## setup

```
setup do
// you setup code
done
```

## loop

```
loop do
// you loop code
done
```

## gpio

```
//                    default   open                          reverse
gpio gpioName pinName [LOW|HIGH [LOW|HIGH]] [-> link attrName [false|true]]
//                    default                       emit
gpio gpioName pinName [LOW|HIGH] [-> click funcName [LOW|HIGH]]
```

## func

```
func funcName [(argv)] do
done
```

## every

```
every funcName delay_ms
```

## command

```
command commandName {
    fn: funcName,
    error: "error info",
    doc: do
markdown doc
done
}
```

## attr

```
attr attrName {
    type: "int",
    min: 0,
    max: 100,
    default: 0,
    scale: 1,
    gen_set: true,
    prec: 2,
    keep: true
}
```

## metric

```
metric metricName {
    type: "float"
    min: 0,
    max: 100,
    min_threshold: 1,
    max_threshold: 50,
    threshold: 1,
    prec: 2
}
```

## rule

```
rule condition do [later do_later_ms ]action [else [later do_later_ms ]elseAction] [on onCondition]
```

# Template Special

README sample compiled syntax.

```yaml
use_eeprom: true
has_gpio: true
setups:
- code: |-
    GL_SERIAL.begin(115200);
        while (!GL_SERIAL) {;}
- code: |-
    MCUSR = 0;
        wdt_disable();
        wdt_enable(WDTO_8S);
commands:
- doc: ''
  error: only relay_mode is 1 can set this value
  flag:
    json: true
    retval: true
  name: set_relay_state
  has_doc: false
  fn: try_set_attr_relay_state
- doc: ''
  error: call reset_system failed
  flag:
    json: false
    retval: false
  name: reset_system
  has_doc: false
  fn: reset_system
loops:
- code: |-
    wdt_reset();
        if (want_reboot) {
            reset();
        }
metrics:
- max: 100.0
  prec: 2
  min_threshold: 1
  addr: 5
  width: 3
  max_threshold: 50.0
  name: temperature
  threshold: 1
  min: 0
  type: float
  threshold_width: 2
max_buf_len: 180
gpios:
- state: LOW
  has_link: true
  link: relay_mode
  close: LOW
  open: HIGH
  name: relay_mode
  pin: LED_BUILTIN
  has_fn: false
  emit: HIGH
  reverse: false
  fn: ''
- state: LOW
  has_link: true
  link: relay_state
  close: LOW
  open: HIGH
  name: relay
  pin: '12'
  has_fn: false
  emit: HIGH
  reverse: false
  fn: ''
- state: HIGH
  has_link: false
  link: ''
  close: HIGH
  open: LOW
  name: btn0
  pin: '11'
  has_fn: true
  emit: LOW
  reverse: false
  fn: try_toggle_gpio_relay
- state: HIGH
  has_link: false
  link: ''
  close: HIGH
  open: LOW
  name: btn1
  pin: '10'
  has_fn: true
  emit: LOW
  reverse: false
  fn: toggle_gpio_relay_mode
rules:
- depends:
  - name: temperature
  on_condition: attr_relay_mode == 0
  has_else_later: true
  has_later: true
  action: open_gpio_relay
  later: attr_open_delay
  has_on: true
  has_else: true
  id: 1
  condition: metric_temperature < attr_high_temperature && metric_temperature > attr_low_temperature
  else_later: attr_close_delay
  else_action: close_gpio_relay
actions:
- delay_ms: 6000
  fn: read_dht
max_req_len: 7
has_float: true
attrs:
- max: 86400.0
  prec: 2
  is_float: false
  scale: 1000.0
  default: 1800000.0
  keep: true
  addr: 1
  width: 5
  scaled_min: 60000.0
  name: delay
  scaled_max: 8.64e7
  min: 60.0
  type: unsigned long
  gen_set: true
- max: 1
  prec: 2
  is_float: false
  scale: 1
  default: 0
  keep: false
  addr: 0
  width: 1
  scaled_min: 0
  name: relay_state
  scaled_max: 1
  min: 0
  type: int
  gen_set: false
- max: 1
  prec: 2
  is_float: false
  scale: 1
  default: 0
  keep: true
  addr: 9
  width: 1
  scaled_min: 0
  name: relay_mode
  scaled_max: 1
  min: 0
  type: int
  gen_set: true
- max: 100.0
  prec: 2
  is_float: true
  scale: 1
  default: 30.0
  keep: true
  addr: 13
  width: 3
  scaled_min: 0
  name: high_temperature
  scaled_max: 100.0
  min: 0
  type: float
  gen_set: true
- max: 100.0
  prec: 2
  is_float: true
  scale: 1
  default: 20.0
  keep: true
  addr: 17
  width: 3
  scaled_min: 0
  name: low_temperature
  scaled_max: 100.0
  min: 0
  type: float
  gen_set: true
- max: 3600.0
  prec: 2
  is_float: false
  scale: 1000.0
  default: 5000.0
  keep: true
  addr: 21
  width: 4
  scaled_min: 0
  name: open_delay
  scaled_max: 3600000.0
  min: 0
  type: unsigned long
  gen_set: true
- max: 3600.0
  prec: 2
  is_float: false
  scale: 1000.0
  default: 5000.0
  keep: true
  addr: 25
  width: 4
  scaled_min: 0
  name: close_delay
  scaled_max: 3600000.0
  min: 0
  type: unsigned long
  gen_set: true
max_tpl_len: 33
has_input: true
consts:
- value: Serial
  name: GL_SERIAL
- value: Serial
  name: DEBUG_SERIAL
- value: attr_delay
  name: METRIC_DELAY_MS
- value: noop
  name: PING_FAILED_CB
app:
  key_len: 8
  token_hex_array: 0x12, 0x34, 0x56, 0x78, 0x90, 0xab, 0xcd, 0xef
  token: 1234567890abcdef
  key: 1234567890abcdef
  name: dht
  context_len: 32
  token_len: 8
  key_hex_array: 0x12, 0x34, 0x56, 0x78, 0x90, 0xab, 0xcd, 0xef
has_app: true
has_debug: true
low_memory: false
max_gl_len: 212
has_rule: true
has_func: true
has_attr: true
functions:
- return: false
  flag:
    json: false
    retval: false
  name: read_dht
  has_argv: false
  argv: ''
  code: |-
    metric_temperature += 0.1;
        if (metric_temperature > 100) {
             metric_temperature = 0;
        }
- return: true
  flag:
    json: true
    retval: true
  name: try_set_attr_relay_state
  has_argv: false
  argv: ''
  code: |-
    if (attr_relay_mode == 1) {
            return set_attr_relay_state(json, tokens, num_tokens, retval);
        }
        return RET_ERR;
- return: false
  flag:
    json: false
    retval: false
  name: try_toggle_gpio_relay
  has_argv: false
  argv: ''
  code: |-
    if (attr_relay_mode == 1) {
            toggle_gpio_relay();
        }
- return: false
  flag:
    json: false
    retval: false
  name: reset
  has_argv: false
  argv: ''
  code: |-
    wdt_disable();
        wdt_enable(WDTO_15MS);
        for (;;) {

        }
- return: false
  flag:
    json: false
    retval: false
  name: reset_system
  has_argv: false
  argv: ''
  code: want_reboot = true;
has_metric: true
inits:
- code: |-
    #include <avr/wdt.h>
    bool want_reboot = false;

```
