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

{=^ has_doc =}

### Command {= name =}

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
{=/ has_doc =}
{=# has_doc =}
{=& doc =}
{=/ has_doc =}

{=/ commands =}

## C API


```c
{=# attrs =}
{= type =} attr_{= name =} = {= default =};
{=/ attrs =}
{=# metrics =}
{= type =} metric_{= name =} = 0;
{=/ metrics =}

unsigned long get_current_time_ms();
{=# use_remote =}
void reset();

void send_packet_0(const uint8_t type);
void send_packet_1(const uint8_t type, const char *data);
void send_packet_rsp(const char *data);

char * ltrim(char *s);
bool jsoneq(const char *json, jsmntok_t *token, const char *s);
int jsonfind(const char *json, jsmntok_t *tokens, int num_tokens, const char *name);
bool jsonlookup(const char *json, jsmntok_t *tokens, int num_tokens, const char *name, char *value);
void merge_json(char *dst, char *src, int *total_length);

{=/ use_remote =}
{=# attrs =}
{=# gen_set =}
int set_attr_{= name =}(const char *json, jsmntok_t *tokens, int num_tokens, char *retval);
{=/ gen_set =}
int get_attr_{= name =}(char *retval);

{=/ attrs =}
{=# metrics =}
int set_metric_{= name =}_threshold(const char *json, jsmntok_t *tokens, int num_tokens, char *retval);
int get_metric_{= name =}_threshold(char *retval);
int get_metric_{= name =}(char *retval);

{=/ metrics =}
{=# gpios =}
{=^ has_fn =}
void open_gpio_{= name =}();

void close_gpio_{= name =}();

void toggle_gpio_{= name =}();

{=/ has_fn =}
{=/ gpios =}
{=# functions =}
{=# flag =}
{=# retval =}
{=# json =}
int {= name =}(const char *json, jsmntok_t *tokens, int num_tokens, char *retval);
{=/ json =}
{=^ json =}
int {= name =}(char *retval);
{=/ json =}
{=/ retval =}
{=^ retval =}
{=# json =}
int {= name =}(const char *json, jsmntok_t *tokens, int num_tokens);
{=/ json =}
{=^ json =}
int {= name =}();
{=/ json =}
{=/ retval =}
{=/ flag =}
{=/ functions =}
```
