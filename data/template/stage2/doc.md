{{={= =}=}}
{=# app =}
# App {= name =}
{=/ app =}

## Commands
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
