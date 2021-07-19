{{={= =}=}}
{=# app =}
# App {= name =}
{=/ app =}

## Commands

{=# attrs =}

{=# gen_set =}
### Edit attribute {= name =}

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

{=/ gen_set =}
### Get attribute {= name =}

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

### Edit metric {= name =} report threshold

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

### Get metric {= name =} report threshold

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

### Get metric {= name =}

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

## Custom commands

{=# commands =}
{=# docs =}
### {= name =}

{=# command =}
@Command:

```json
{=& payload =}
```
{=# has_doc =}
{=# docs =}
- {=& . =}
{=/ docs =}
{=/ has_doc =}
{=/ command =}

{=# return =}
@Return:

```json
{=& payload =}
```
{=# has_doc =}
{=# docs =}
- {=& . =}
{=/ docs =}
{=/ has_doc =}
{=/ return =}

{=# error =}
@Error:

```json
{=& payload =}
```
{=# has_doc =}
{=# docs =}
- {=& . =}
{=/ docs =}
{=/ has_doc =}
{=/ error =}
{=/ docs =}

{=/ commands =}

## C API


```c
{=# imports =}
#include {=& name =}
{=/ imports =}

{=# consts =}
{=# has_value =}
#define {= name =} {= value =}
{=/ has_value =}
{=^ has_value =}
#define {= name =}
{=/ has_value =}
{=/ consts =}

{=# vars =}
{=# has_value =}
{= type =} {= name =} = {= value =};
{=/ has_value =}
{=^ has_value =}
{= type =} {= name =};
{=/ has_value =}
{=/ vars =}
unsigned long current_time_ms = 0;

unsigned long get_current_time_ms();
{=# functions =}
{= type =} {= name =}({=# argv =}{= type =} {= name =}{=^ last =}, {=/ last =}{=/ argv =});
{=/ functions =}
```
