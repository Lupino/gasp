# Language Special
## app

```
app appName {
    key: "you_key",
    token: "you_token"
}
```

## init

```
init {=code
// you init code
code=}
```

## setup

```
setup {=code
// you setup code
code=}
```

## loop

```
loop {=code
// you loop code
code=}
```

## gpio

```
gpio gpioName {
    pin: "LED_BUILTIN",
    link: attrName,
    fn: funcName,
    emit: HIGH,
    state: LOW,
    open: HIGH,
    close: LOW
    reverse: false
}
```

## func

```
func funcName {=code
code=}
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
    doc: {=md
markdown doc
md=}
}
```

## telemetry

```
telemetry funcName
```

## attr

```
attr attrName {
    var: attrName,
    type: "int",
    min: 0,
    max: 100,
    default: 0,
    scale: 1,
    gen_set: true
}
```

## metric

```
metric metricName {
    var: metricName,
    type: "float"
    min: 0,
    max: 100,
    min_threshold: 1,
    max_threshold: 50,
    threshold: 1,
    prec: 2
}
```

# Template Special

```
{
    "app": {
        "name": "appName",
        "key": "you_key",
        "token": "you_token"
    },
    "commands": [
        {
            "name": "commandName",
            "fn": "funcName",
            "flag": {
                "json": false,
                "retval": true
            },
            "doc": "markdown doc\n",
            "has_doc": true
        }
    ],
    "telemetries": [
        {
            "fn": "funcName",
            "flag": {
                "json": false,
                "retval": true
            },
        }
    ],
    "functions": [
        {
            "name": "funcName",
            "code": "// you func code\n",
            "flag": {
                "json": false,
                "retval": true
            },
            "return": false
        }
    ],
    "loops": [
        {
            "code": "// you loop code\n"
        }

    ],
    "setups": [
        {
            "code": "// you setup code\n"
        }
    ],
    "inits": [
        {
            "code": "// you init code\n"
        }
    ],
    "attrs": [
        {
            "name": "attrName",
            "addr": 1,
            "var": "attrName",
            "max": 100,
            "min": 0,
            "scaled_max": 100,
            "scaled_min": 0,
            "scale": 1,
            "type": "int",
            "gen_set": true,
            "default": 0
        }
    ],
    "metrics": [
        {
            "name": "metricName",
            "addr": 5,
            "var": "metricName",
            "max": 100,
            "min": 0,
            "max_threshold": 50,
            "min_threshold": 1,
            "threshold": 1,
            "threshold_width": 2,
            "type": "float",
            "width": 3,
            "prec": 2
        }
    ],
    "actions": [
        {
            "fn": "funcName",
            "delay_ms": 5
        }
    ],
    "gpios": [
        {
            "name": "gpioName",
            "pin": "LED_BUILTIN",
            "fn": "funcName",
            "link": "attrVar",
            "emit": "HIGH",
            "state": "LOW",
            "open": "HIGH",
            "close": "LOW",
            "reverse": false,
            "has_link": true,
            "has_fn": false
        }
    ],
    "has_attr": true,
    "has_metric": true,
    "use_eeprom": true,
    "max_cmd_len": 29,
    "has_gpio": true,
    "has_func": true,
    "has_input": true,
    "use_remote": true,
    "has_debug": true
}
```
