# App l76x

## Commands


### Edit attribute delay

@Command:

```json
{
    "method": "set_delay",
    "data": 60
}
```

- data is between [60, 86400]

@Return:

```json
{
    "delay": 60
}
```

- delay is between [60, 86400]

@Error:

```json
{
    "err": "data must between: [60, 86400]"
}
```

### Get attribute delay

@Command:

```json
{
    "method": "get_delay"
}
```

@Return:

```json
{
    "delay": 60
}
```

- delay is between [60, 86400]


### Edit metric lat report threshold

@Command:

```json
{
    "method": "set_lat_threshold",
    "data": 1.0e-5
}
```

- data is between [1.0e-5, 1]

@Return:

```json
{
    "lat_threshold": 1.0e-5
}
```

- lat_threshold is between [1.0e-5, 1]

@Error:

```json
{
    "err": "data must between: [1.0e-5, 1]"
}
```

### Get metric lat report threshold

@Command:

```json
{
    "method": "get_lat_threshold"
}
```

@Return:

```json
{
    "lat_threshold": 1.0e-5
}
```

- lat_threshold is between [1.0e-5, 1]

### Get metric lat

@Command:

```json
{
    "method": "get_lat"
}
```

@Return:

```json
{
    "lat": -180
}
```

- lat is between [-180, 180]

@Error:

```
{
    "err": "lat is inviald"
}
```

### Edit metric lon report threshold

@Command:

```json
{
    "method": "set_lon_threshold",
    "data": 1.0e-5
}
```

- data is between [1.0e-5, 1]

@Return:

```json
{
    "lon_threshold": 1.0e-5
}
```

- lon_threshold is between [1.0e-5, 1]

@Error:

```json
{
    "err": "data must between: [1.0e-5, 1]"
}
```

### Get metric lon report threshold

@Command:

```json
{
    "method": "get_lon_threshold"
}
```

@Return:

```json
{
    "lon_threshold": 1.0e-5
}
```

- lon_threshold is between [1.0e-5, 1]

### Get metric lon

@Command:

```json
{
    "method": "get_lon"
}
```

@Return:

```json
{
    "lon": -180
}
```

- lon is between [-180, 180]

@Error:

```
{
    "err": "lon is inviald"
}
```

## Custom commands

