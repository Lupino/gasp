{{={= =}=}}
{=# app =}
# App {= name =}
{=/ app =}

# Commands

{=# attrs =}

## Edit attribute {= name =}

@Command:

```json
{
    "method": "set_{= name =}",
    "data": {= min =}
}
```

- data is between [{= min =}, {= max =}]

@Return:

```json
{
    "{= name =}": {= min =}
}
```

- {= name =} is between [{= min =}, {= max =}]

@Error:

```json
{
    "err": "data must between: [{= min =}, {= max =}]"
}
```

## Get attribute {= name =}

@Command:

```json
{
    "method": "get_{= name =}"
}
```

@Return:

```json
{
    "{= name =}": {= min =}
}
```

- {= name =} is between [{= min =}, {= max =}]

{=/ attrs =}
{=# metrics =}

## Edit metric {= name =} report threshold

@Command:

```json
{
    "method": "set_{= name =}_threshold",
    "data": {= threshold =}
}
```

- data is between [{= min_threshold =}, {= max_threshold =}]

@Return:

```json
{
    "{= name =}_threshold": {= threshold =}
}
```

- {= name =}_threshold is between [{= min_threshold =}, {= max_threshold =}]

@Error:

```json
{
    "err": "data must between: [{= min_threshold =}, {= max_threshold =}]"
}
```

## Get metric {= name =} report threshold

@Command:

```json
{
    "method": "get_{= name =}_threshold"
}
```

@Return:

```json
{
    "{= name =}_threshold": {= threshold =}
}
```

- {= name =}_threshold is between [{= min_threshold =}, {= max_threshold =}]

## Get metric {= name =}

@Command:

```json
{
    "method": "get_{= name =}"
}
```

@Return:

```json
{
    "{= name =}": {= min =}
}
```

- {= name =} is between [{= min =}, {= max =}]

@Error:

```
{
    "err": "{= name =} is inviald"
}
```
{=/ metrics =}

# Custom commands

{=# commands =}

@Command:

```json
{
    "method": "{= name =}"
}
```

@Return:

```json
{
    "result": "OK"
}
```

@Error:

```json
{
    "err": "{= error =}"
}
```
{=/ commands =}
