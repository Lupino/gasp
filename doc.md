# Language Special
## app

```
app appName {
    key: "you_key",
    token: "you_token",
    addr: "00000000",
    start_addr: 0
}
```

only the first one avaliable

## flag

```
flag flagName true|false
```

Avaliable flags:
- has_debug
- ctrl_mode
- auto_retry
- low_memory

Relpace able only the first one avaliable

## fd

```
fd int funcName
```

Relpace able only the first one avaliable

## Constant or Variable

```
name[(argv)] [type] = value
```

Relpace able only the first one avaliable

## import

```
import lib [url]
```

Relpace able only the first one avaliable

## setup or setup1

```
setup name {
// you setup code
}

setup1 name {
// you setup1 code
}
```

Relpace able only the first one avaliable

sorted render

## loop or loop1

```
loop name {
// you loop code
}

loop1 name {
// you loop1 code
}
```

Relpace able only the first one avaliable

sorted render

## raw

```
raw name {
// you raw code
}
```

Relpace able only the first one avaliable

sorted render

## data

```
data name {
// you yaml code
}
```

Relpace able only the first one avaliable

## tmpl

```
tmpl tmplName {
// you tmpl code
}
```

defined tmpl code

## render

```
render tmplName
```

render tmpl code by global gasp exprs

## render1

```
render1 tmplName {
// yaml code
}
```

render tmpl code by yaml data

## gpio

```
//                    default   open                          reverse
gpio gpioName pinName [LOW|HIGH [LOW|HIGH]] [-> link attrName [false|true]]
//                    default                       emit
gpio gpioName pinName [LOW|HIGH] [-> click funcName [LOW|HIGH]]
//                     default
gpio gpioName pinName [NUM] [-> pwm attrName]
```

## agpio

```
agpio agpioName pinName [-> link attrName]
```

## func

```
func funcName [(arg type)] [type] {
    // you func code
}
```

Relpace able only the first one avaliable

## every

```
every [core0|core1] funcName delay_ms [on condition]
```

## command

```
command commandName {
    fn: funcName,
    error: "error info",
    docs: {
        name: "docName",
        command: {
            docs: [
                - some yaml doc
            ],
            payload: {
                method: some method
                # yaml code
            }
        },
        return: {
            docs: [
                - some yaml doc
            ],
            payload: {
                # yaml code
                some_key: some_value
            }
        },
        error: {
            docs: [
                - some yaml doc
            ],
            payload: {
                # yaml code
                err: some error
            }
        }
    }
}
```

## attr

```
attr attrName {
    type: int,
    min: 0,
    max: 100,
    default: 0,
    scale: 1,
    gen_set: true,
    prec: 2,
    readed: true,
    keep: true
}
```

## metric

```
metric metricName {
    type: float
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
rule [core0|core1] condition do [later do_later_ms ]action [else [later do_later_ms ]elseAction] [on onCondition]
```

## uart

```
uart [core0|core1] uartName {
    write writeName ["1234"|fnGen bufLen] [on condition]  // hex command or gen function
    read bufLen fnReadName fnParseName [on condition]
}
```

## require

```
require "path to part gasp file"
```


## timer

```
timer timerName callFuncName finishFuncName
```


## linkage

```
linkage [core0|core1] linkageName getSensorValue openFuncName closeFuncName
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
- error: only relay_mode is 1 can set this value
  flag:
    json: true
    retval: true
  name: set_relay_state
  fn: try_set_attr_relay_state
  docs:
    command:
      payload: '{"data":0,"method":"set_relay_state,"}'
      has_doc: true
      payload_yaml: |-
        data: 0
              method: set_relay_state,
      docs:
      - data is between [0, 1]
    return:
      payload: '{"relay_state":0}'
      has_doc: true
      payload_yaml: 'relay_state: 0'
      docs:
      - relay_state is between [0, 1]
    error:
      payload: '{"err":"only relay_mode is 1 can set this value"}'
      has_doc: false
      payload_yaml: 'err: only relay_mode is 1 can set this value'
      docs: []
    name: Edit attribute relay_state
- error: call reset_system failed
  flag:
    json: false
    retval: false
  name: reset_system
  fn: reset_system
  docs:
    command:
      payload: '{"method":"reset_system"}'
      has_doc: false
      payload_yaml: 'method: reset_system'
      docs: []
    return:
      payload: '{"result":"OK"}'
      has_doc: false
      payload_yaml: 'result: OK'
      docs: []
    error:
      payload: '{"err":"call reset_system failed"}'
      has_doc: false
      payload_yaml: 'err: call reset_system failed'
      docs: []
    name: Command reset_system
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
  addr: 16
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
- state: LOW
  has_link: false
  link: ''
  close: LOW
  open: HIGH
  name: auth
  pin: '9'
  has_fn: true
  emit: HIGH
  reverse: false
  fn: noop
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
- depends: []
  on_condition: givelink_context_authed()
  has_else_later: false
  has_later: true
  action: emit_givelink_unauth
  later: '2000'
  has_on: true
  has_else: true
  id: 2
  condition: gpio_auth_state == HIGH
  else_later: ''
  else_action: allow_emit_givelink_unauth
actions:
- delay_ms: 6000
  fn: read_dht
ctrl_mode: false
max_req_len: 7
has_float: true
attrs:
- max: 86400.0
  prec: 2
  is_float: false
  scale: 1000.0
  default: 1800000.0
  keep: true
  addr: 12
  width: 5
  onebyte: false
  scaled_min: 60000.0
  name: delay
  uncheckmin: false
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
  onebyte: true
  scaled_min: 0
  name: relay_state
  uncheckmin: true
  scaled_max: 1
  min: 0
  type: uint8_t
  gen_set: false
- max: 1
  prec: 2
  is_float: false
  scale: 1
  default: 0
  keep: true
  addr: 20
  width: 1
  onebyte: true
  scaled_min: 0
  name: relay_mode
  uncheckmin: true
  scaled_max: 1
  min: 0
  type: uint8_t
  gen_set: true
- max: 100.0
  prec: 2
  is_float: true
  scale: 1
  default: 30.0
  keep: true
  addr: 21
  width: 3
  onebyte: false
  scaled_min: 0
  name: high_temperature
  uncheckmin: false
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
  addr: 25
  width: 3
  onebyte: false
  scaled_min: 0
  name: low_temperature
  uncheckmin: false
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
  addr: 29
  width: 4
  onebyte: false
  scaled_min: 0
  name: open_delay
  uncheckmin: true
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
  addr: 33
  width: 4
  onebyte: false
  scaled_min: 0
  name: close_delay
  uncheckmin: true
  scaled_max: 3600000.0
  min: 0
  type: unsigned long
  gen_set: true
max_tpl_len: 51
production: true
has_input: true
consts:
- value: Serial
  name: GL_SERIAL
- value: Serial
  name: DEBUG_SERIAL
- value: attr_delay
  name: METRIC_DELAY_MS
app:
  addr_hex_array: 0x00, 0x00, 0x00, 0x00
  key_len: 8
  addr_addr: 8
  token_hex_array: 0x12, 0x34, 0x56, 0x78, 0x90, 0xab, 0xcd, 0xef
  token_addr: 0
  token: 1234567890abcdef
  addr: '00000000'
  key: 1234567890abcdef
  name: dht
  context_len: 32
  token_len: 8
  key_hex_array: 0x12, 0x34, 0x56, 0x78, 0x90, 0xab, 0xcd, 0xef
  addr_len: 4
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
        return false;
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
- return: false
  flag:
    json: false
    retval: false
  name: emit_givelink_unauth
  has_argv: false
  argv: ''
  code: |-
    if (can_emit_givelink_unauth) {
            can_emit_givelink_unauth = false;
            givelink_context_set_auth(false);
        }
- return: false
  flag:
    json: false
    retval: false
  name: allow_emit_givelink_unauth
  has_argv: false
  argv: ''
  code: can_emit_givelink_unauth = true;
has_metric: true
inits:
- code: |-
    #include <avr/wdt.h>
    bool want_reboot = false;
- code: bool can_emit_givelink_unauth = false;

```
