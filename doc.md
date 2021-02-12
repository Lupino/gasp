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
    prec: 2
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
rule condition do action [else elseAction]
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
    "functions": [
        {
            "name": "funcName",
            "code": "// you func code\n",
            "argv": "",
            "has_argv": false,
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
            "max": 100,
            "min": 0,
            "scaled_max": 100,
            "scaled_min": 0,
            "scale": 1,
            "type": "int",
            "gen_set": true,
            "default": 0,
            "is_float": false,
            "prec": 2,
            "width": 3
        }
    ],
    "metrics": [
        {
            "name": "metricName",
            "addr": 5,
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
            "link": "attrName",
            "emit": "HIGH",
            "state": "LOW",
            "open": "HIGH",
            "close": "LOW",
            "reverse": false,
            "has_link": true,
            "has_fn": false
        }
    ],
    "rules": [
        {
            "condition": "condition",
            "action": "action",
            "else_action": "",
            "has_else": false,
            "depends": []
        }
    ],
    "has_attr": true,
    "has_metric": true,
    "use_eeprom": true,
    "max_cmd_len": 29,
    "has_gpio": true,
    "has_func": true,
    "has_input": true,
    "has_app": true,
    "has_debug": true
}
```
