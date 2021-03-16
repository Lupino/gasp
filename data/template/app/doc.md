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
{=# attrs =}
{= type =} attr_{= name =} = {= default =};
{=/ attrs =}
{=# metrics =}
{= type =} metric_{= name =} = 0;
{=/ metrics =}

{=# gpios =}
{=# bind =}
{=# is_link =}
uint8_t gpio_{= name =}_state = {= state =};
{=/ is_link =}
{=# is_fn =}
uint8_t gpio_{= name =}_state = {= state =};
{=/ is_fn =}
{=# is_no_bind =}
uint8_t gpio_{= name =}_state = {= state =};
{=/ is_no_bind =}
{=# is_pwm =}
uint8_t gpio_{= name =}_state = {= state =};
{=/ is_pwm =}
{=/ bind =}
{=/ gpios =}
{=# agpios =}
{=# bind =}
{=# is_no_bind =}
uint16_t agpio_{= name =}_value = 0;
{=/ is_no_bind =}
{=/ bind =}
{=/ agpios =}
{=# uarts =}
SoftwareSerial uart_{= name =}({= rx =}, {= tx =});
{=# readers =}
uint8_t uart_{= name =}_read_{= rname =}_buffer[{= buf_len =}];
int uart_{= name =}_read_{= rname =}_buffer_len = 0;
{=/ readers =}
{=# writers =}
bool is_uart_{= name =}_write_{= wname =} = false;
{=/ writers =}
{=/ uarts =}

unsigned long get_current_time_ms();

{=# has_float =}
bool is_valid_float(float number, float min, float max);
{=/ has_float =}
{=# has_app =}
void mainAction();
void noop();
void send_packet();
void send_packet_0(const uint8_t type);
void send_packet_1(const uint8_t type, const char *data);
void send_packet_rsp(const char *data);
void next_packet(const uint8_t type);
char * FC(const __FlashStringHelper *ifsh);
char * ltrim(char *s);
bool jsoneq(const char *json, jsmntok_t *token, const char *s);
int jsonfind(const char *json, jsmntok_t *tokens, int num_tokens, const char *name);

bool jsonlookup(const char *json, jsmntok_t *tokens, int num_tokens, const char *name, char *value);
void merge_json(char *dst, char *src, int *total_length);
{=# attrs =}
void set_attr_{= name =}_raw({= type =} unscaled_value);
bool set_attr_{= name =}(const char *json, jsmntok_t *tokens, int num_tokens, char *retval);
bool get_attr_{= name =}(char *retval);

{=/ attrs =}
{=# metrics =}
{=# auto =}
bool set_metric_{= name =}_threshold(const char *json, jsmntok_t *tokens, int num_tokens, char *retval);
bool get_metric_{= name =}_threshold(char *retval);
{=/ auto =}
bool check_metric_{= name =}();
bool invalid_metric_{= name =}_error(char *retval);
bool get_metric_{= name =}(char *retval);

{=/ metrics =}
{=/ has_app =}
{=# gpios =}
{=# bind =}
{=# is_link =}
void open_gpio_{= name =}_raw();
void close_gpio_{= name =}_raw();
void open_gpio_{= name =}();
void close_gpio_{= name =}();
void toggle_gpio_{= name =}();

{=/ is_link =}
{=# is_no_bind =}
void open_gpio_{= name =}();
void close_gpio_{= name =}();
void toggle_gpio_{= name =}();

{=/ is_no_bind =}
{=/ bind =}
{=/ gpios =}
{=# functions =}
{=# has_argv =}
bool {= name =}({= argv =});
{=/ has_argv =}
{=^ has_argv =}
{=# flag =}
{=# retval =}
{=# json =}
bool {= name =}(const char *json, jsmntok_t *tokens, int num_tokens, char *retval);
{=/ json =}
{=^ json =}
bool {= name =}(char *retval);
{=/ json =}
{=/ retval =}
{=^ retval =}
{=# json =}
bool {= name =}(const char *json, jsmntok_t *tokens, int num_tokens);
{=/ json =}
{=^ json =}
bool {= name =}();
{=/ json =}
{=/ retval =}
{=/ flag =}
{=/ has_argv =}

{=/ functions =}
{=# has_app =}
bool processRequest(const char *json, int length, char *retval);
{=# has_metric =}
bool reportMetric(bool force);
{=/ has_metric =}
{=# use_eeprom =}
bool reportAttribute(bool force);
{=/ use_eeprom =}
{=/ has_app =}
{=# uarts =}
{=# writers =}
void uart_{= name =}_write_{= wname =}();
{=/ writers =}
void uart_{= name =}_write();
{=/ uarts =}
```
