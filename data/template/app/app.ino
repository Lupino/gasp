{{={= =}=}}
{=# has_app =}
#include <givelink.h>
#include <jsmn.h>

{=/ has_app =}
{=# use_eeprom =}
#include <EEPROM.h>

{=/ use_eeprom =}
#define RET_ERR -1
#define RET_SUCC 0

{=# inits =}
{=& code =}

{=/ inits =}
{=# has_app =}
unsigned long auth_timer_ms = 0;

#ifndef AUTH_DELAY_MS
#define AUTH_DELAY_MS 1000
#endif

#ifndef PONG_DELAY_MS
#define PONG_DELAY_MS 6000
#endif

#ifndef PING_DELAY_MS
#define PING_DELAY_MS 300000
#endif

#ifndef PING_FAILED_CB
#define PING_FAILED_CB noop
#endif

#ifndef MAX_PING_FAILED
#define MAX_PING_FAILED 10
#endif

unsigned long pong_timer_ms = 0;
unsigned long ping_timer_ms = 0;
bool ponged = true;
int ping_failed = 0;

#ifndef MAX_GL_PAYLOAD_LENGTH
#define MAX_GL_PAYLOAD_LENGTH 127
#endif
#ifndef WANT_SEND_DATA_LENGTH
#define WANT_SEND_DATA_LENGTH 50
#endif

#ifndef MAX_NUM_TOKENS
#define MAX_NUM_TOKENS 10
#endif

#ifndef MAX_REQUEST_VALUE_LENGTH
#define MAX_REQUEST_VALUE_LENGTH {= max_cmd_len =}
#endif

givelink_context_t ctx0;
givelink_context_t * ctx;
{=# app =}
uint8_t ctx_buff[{= context_len =}];
const uint8_t key[{= key_len =}] = {{= key_hex_array =}};
const uint8_t token[{= token_len =}] = {{= token_hex_array =}};
{=/ app =}

givelink_t m0;
givelink_t * m;
uint8_t m_buff[MAX_GL_PAYLOAD_LENGTH];

uint16_t lastPayloadId = 0;
uint16_t readedLen = 0;
uint8_t  readedPayload[MAX_GL_PAYLOAD_LENGTH + 1];
uint8_t  sendedPayload[MAX_GL_PAYLOAD_LENGTH + 1];


jsmn_parser requestJsmnParser;
jsmntok_t requestJsmnTokens[MAX_NUM_TOKENS]; /* We expect no more than 128 tokens */
char requestValue[MAX_REQUEST_VALUE_LENGTH];
char wantSendData[WANT_SEND_DATA_LENGTH];
char tempSendData[WANT_SEND_DATA_LENGTH];
char wantSendDataTpl[WANT_SEND_DATA_LENGTH];

{=# has_metric =}
#ifndef METRIC_DELAY_MS
#define METRIC_DELAY_MS 1800000
#endif

unsigned long metric_timer_ms = 0;

{=/ has_metric =}
{=# use_eeprom =}
bool requireReportAttribute = false;
bool requireReportMetric = true;

{=/ use_eeprom =}
{=/ has_app =}
{=# attrs =}
{= type =} attr_{= name =} = {= default =};
{=# has_app =}
{= type =} last_attr_{= name =} = {= default =};
{=/ has_app =}

{=/ attrs =}
{=# metrics =}
{= type =} metric_{= name =} = 0;
{=# has_app =}
{= type =} last_metric_{= name =} = 0;
{= type =} metric_{= name =}_threshold = {= threshold =};
{= type =} last_metric_{= name =}_threshold = {= threshold =};
{=/ has_app =}

{=/ metrics =}
{=# actions =}
unsigned long {= fn =}_timer_ms = 0;
{=/ actions =}

{=# has_rule =}
bool rule_depends_checked = false;
{=/ has_rule =}
{=# has_input =}
#ifndef DEBOUNCE_DELAY_MS
#define DEBOUNCE_DELAY_MS 50
#endif
int gpio_reading = 0;
{=/ has_input =}
{=# has_gpio =}
{=# gpios =}
int gpio_{= name =}_pin = {= pin =};
int gpio_{= name =}_state = {= state =};
{=# has_fn =}
unsigned long last_gpio_{= name =}_debounce_time_ms = 0;
int last_gpio_{= name =}_state = {= state =};
{=/ has_fn =}

{=/ gpios =}
{=/ has_gpio =}
// defined
unsigned long get_current_time_ms();

{=# has_app =}
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
int set_attr_{= name =}(const char *json, jsmntok_t *tokens, int num_tokens, char *retval);
int get_attr_{= name =}(char *retval);

{=/ attrs =}
{=# metrics =}
int set_metric_{= name =}_threshold(const char *json, jsmntok_t *tokens, int num_tokens, char *retval);
int get_metric_{= name =}_threshold(char *retval);
bool check_metric_{= name =}();
int invalid_metric_{= name =}_error(char *retval);
int get_metric_{= name =}(char *retval);

{=/ metrics =}
{=/ has_app =}
{=# gpios =}
{=^ has_fn =}
{=# has_link =}
void open_gpio_{= name =}_raw();
void close_gpio_{= name =}_raw();
void open_gpio_{= name =}();
void close_gpio_{= name =}();
{=/ has_link =}
{=^ has_link =}
void open_gpio_{= name =}();
void close_gpio_{= name =}();
{=/ has_link =}
void toggle_gpio_{= name =}();
{=/ has_fn =}
{=/ gpios =}
{=# functions =}
{=# has_argv =}
int {= name =}({= argv =});
{=/ has_argv =}
{=^ has_argv =}
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
{=/ has_argv =}

{=/ functions =}
{=# has_app =}
int processRequest(const char *json, int length, char *retval);
{=# has_metric =}
bool reportMetric(bool force);
{=/ has_metric =}
{=# use_eeprom =}
bool reportAttribute(bool force);
{=/ use_eeprom =}
{=/ has_app =}
// end defined
void setup() {
    {=# has_app =}
    {=# app =}
    ctx = givelink_context_init(&ctx0, ctx_buff);
    givelink_context_set_key(ctx, key, {= key_len =});
    givelink_context_set_token(ctx, token, {= token_len =});
    givelink_set_auth(ctx, false);
    m = givelink_init(&m0, m_buff);
    {=/ app =}

    {=/ has_app =}
    {=# use_eeprom =}
    byte first_run_flag = 0;
    EEPROM.get(0, first_run_flag);

    {=# attrs =}
    {=# keep =}
    if (first_run_flag == 1) {
        EEPROM.get({= addr =}, attr_{= name =});
        if (attr_{= name =} > {= scaled_max =} || attr_{= name =} < {= scaled_min =}) {
            attr_{= name =} = {= default =};
        }
    } else {
        EEPROM.put({= addr =}, attr_{= name =});
    }

    {=/ keep =}
    {=/ attrs =}
    {=# has_app =}
    {=# metrics =}
    if (first_run_flag == 1) {
        EEPROM.get({= addr =}, metric_{= name =}_threshold);
        if (metric_{= name =}_threshold > {= max_threshold =} || metric_{= name =}_threshold < {= min_threshold =}) {
            metric_{= name =}_threshold = {= threshold =};
        }
    } else {
        EEPROM.put({= addr =}, metric_{= name =}_threshold);
    }

    {=/ metrics =}
    {=/ has_app =}
    if (first_run_flag != 0) {
      first_run_flag = 1;
      EEPROM.put(0, first_run_flag);
    }

    {=/ use_eeprom =}
    {=# has_gpio =}
    {=# gpios =}
    {=# has_fn =}
    pinMode(gpio_{= name =}_pin, INPUT);
    {=/ has_fn =}
    {=^ has_fn =}
    pinMode(gpio_{= name =}_pin, OUTPUT);
    {=/ has_fn =}

    {=/ gpios =}
    {=/ has_gpio =}
    {=# setups =}
    {=& code =}

    {=/ setups =}
    {=# has_app =}
    {=# has_debug =}
    #ifdef DEBUG_SERIAL
    DEBUG_SERIAL.println(F("Setup"));
    #endif
    {=/ has_debug =}
    {=/ has_app =}
}

void loop() {
    {=# loops =}
    {=& code =}
    {=/ loops =}
    {=# rules =}
    rule_depends_checked = true;
    {=# depends =}
    if (!check_metric_{= name =}()) {
        rule_depends_checked = false;
    }
    {=/ depends =}
    if (rule_depends_checked) {
        if ({=& condition =}) {
            {= action =}();
        {=# has_else =}
        } else {
            {= else_action =}();
        {=/ has_else =}
        }
    }
    {=/ rules =}
    {=# has_app =}
    while (GL_SERIAL.available() > 0) {
        uint8_t outByte = GL_SERIAL.read();
        if (givelink_recv(ctx, readedPayload, &readedLen, outByte)) {
            if (givelink_from_binary(ctx, m, readedPayload, readedLen)) {
                {=# has_debug =}
                #ifdef DEBUG_SERIAL
                DEBUG_SERIAL.print(F("Recv Id: "));
                DEBUG_SERIAL.print(m -> id);
                DEBUG_SERIAL.print(F(" Type: "));
                DEBUG_SERIAL.print(m -> type);
                if (m -> length > TYPE_LENGTH) {
                    DEBUG_SERIAL.print(F(" Data: "));
                    for (uint16_t i = 0; i < m -> length - TYPE_LENGTH; i ++) {
                        DEBUG_SERIAL.write(m -> data[i]);
                    }
                }
                DEBUG_SERIAL.println();
                #endif

                {=/ has_debug =}
                if (m -> type == REQUEST) {
                    wantSendData[0] = '\0';
                    int ret = processRequest((const char *)m -> data, m -> length - TYPE_LENGTH, wantSendData);
                    if (wantSendData[0] == '\0') {
                        if (ret > RET_ERR) {
                            sprintf(wantSendData, FC(F("{\"result\": \"OK\"}")));
                        } else {
                            sprintf(wantSendData, FC(F("{\"err\": \"not support\"}")));
                        }
                    }
                    send_packet_rsp(wantSendData);
                }
                if (m -> type == SUCCESS) {
                    ponged = true;
                }
            }
            readedLen = 0;
        }
        if (readedLen > MAX_GL_PAYLOAD_LENGTH) {
            {=# has_debug =}
            #ifdef DEBUG_SERIAL
            DEBUG_SERIAL.println(F("Error: payload to large"));
            #endif
            {=/ has_debug =}
            readedLen = 0;
        }
    }

    if (givelink_authed(ctx)) {
        {=# use_eeprom =}
        reportAttribute(requireReportAttribute);
        if (requireReportAttribute) {
            requireReportAttribute = false;
        }
        {=/ use_eeprom =}
        {=# has_metric =}
        if (metric_timer_ms + METRIC_DELAY_MS < get_current_time_ms()) {
            requireReportMetric = true;
        }
        if (reportMetric(requireReportMetric)) {
            metric_timer_ms = get_current_time_ms();
            requireReportMetric = false;
        }
        {=/ has_metric =}

        if (ping_timer_ms + PING_DELAY_MS < get_current_time_ms()) {
            send_packet_0(PING);
            ponged = false;
            ping_timer_ms = get_current_time_ms();
            pong_timer_ms = get_current_time_ms();
        }

        if (ponged) {
            ping_failed = 0;
        } else {
            if (pong_timer_ms + PONG_DELAY_MS < get_current_time_ms()) {
                ping_failed += 1;
                pong_timer_ms = get_current_time_ms();

                if (ping_failed > MAX_PING_FAILED) {
                    PING_FAILED_CB();
                }
            }
        }
    } else {
        {=# use_eeprom =}
        requireReportAttribute = true;
        {=/ use_eeprom =}
        if (auth_timer_ms + AUTH_DELAY_MS < get_current_time_ms()) {
            send_packet_0(AUTHREQ);
            auth_timer_ms = get_current_time_ms();
        }
    }

    {=/ has_app =}
    {=# actions =}
    if ({= fn =}_timer_ms + {= delay_ms =} < get_current_time_ms()) {
        {= fn =}();
        {= fn =}_timer_ms = get_current_time_ms();
    }
    {=/ actions =}
    {=# has_gpio =}
    {=# gpios =}
    {=# has_fn =}
    gpio_reading = digitalRead(gpio_{= name =}_pin);
    if (gpio_reading != last_gpio_{= name =}_state) {
        last_gpio_{= name =}_debounce_time_ms = get_current_time_ms();
    }
    if ((get_current_time_ms() - last_gpio_{= name =}_debounce_time_ms) > DEBOUNCE_DELAY_MS) {
        if (gpio_reading != gpio_{= name =}_state) {
          gpio_{= name =}_state = gpio_reading;
          if (gpio_{= name =}_state == {= emit =}) {
            {= fn =}();
          }
        }
    }
    last_gpio_{= name =}_state = gpio_reading;
    {=/ has_fn =}
    {=# has_link =}
    {=# reverse =}
    if (attr_{= link =} == gpio_{= name =}_state) {
        if (attr_{= link =} == {= open =}) {
            close_gpio_{= name =}_raw();
        } else {
            open_gpio_{= name =}_raw();
        }
    }
    {=/ reverse =}
    {=^ reverse =}
    if (attr_{= link =} != gpio_{= name =}_state) {
        if (attr_{= link =} == {= open =}) {
            open_gpio_{= name =}_raw();
        } else {
            close_gpio_{= name =}_raw();
        }
    }
    {=/ reverse =}
    {=/ has_link =}

    {=/ gpios =}
    {=/ has_gpio =}
}

unsigned long get_current_time_ms() {
    return millis();
}

{=# has_app =}
void noop() {}

void send_packet() {
    {=# has_debug =}
    #ifdef DEBUG_SERIAL
    DEBUG_SERIAL.print(F("Send Id: "));
    DEBUG_SERIAL.print(m -> id);
    DEBUG_SERIAL.print(F(" Type: "));
    DEBUG_SERIAL.print(m -> type);
    if (m -> length > TYPE_LENGTH) {
        DEBUG_SERIAL.print(F(" Data: "));
        for (uint16_t i = 0; i < m -> length - TYPE_LENGTH; i ++) {
            DEBUG_SERIAL.write(m -> data[i]);
        }
    }
    DEBUG_SERIAL.println();
    #endif
    {=/ has_debug =}
    givelink_to_binary(ctx, m, sendedPayload);
    uint16_t length = givelink_get_length(ctx, m);
    for (uint16_t i = 0; i < length; i ++) {
        GL_SERIAL.write(sendedPayload[i]);
    }
    GL_SERIAL.write('\r');
    GL_SERIAL.write('\n');
}

void send_packet_0(const uint8_t type) {
    next_packet(type);
    send_packet();
}

void send_packet_1(const uint8_t type, const char *data) {
    next_packet(type);
    givelink_set_data(m, (const uint8_t*)data, strlen(data));
    send_packet();
}

void send_packet_rsp(const char *data) {
    givelink_set_type(m, RESPONSE);
    givelink_set_data(m, (const uint8_t*)wantSendData, strlen(data));
    send_packet();
}

void next_packet(const uint8_t type) {
    givelink_reset(m);
    givelink_set_id(m, lastPayloadId);
    givelink_set_type(m, type);
    lastPayloadId ++;
}

char * FC(const __FlashStringHelper *ifsh) {
    PGM_P p = reinterpret_cast<PGM_P>(ifsh);
    size_t n = 0;
    while (1) {
        unsigned char c = pgm_read_byte(p++);
        wantSendDataTpl[n] = c;
        n++;
        if (c == 0) break;
    }
    return wantSendDataTpl;
}

char * ltrim(char *s) {
  while (*s == ' ') s++;
  return s;
}

bool jsoneq(const char *json, jsmntok_t *token, const char *s) {
  if (token->type == JSMN_STRING && (int)strlen(s) == token->end - token->start &&
      strncmp(json + token->start, s, token->end - token->start) == 0) {
    return true;
  }
  return false;
}

int jsonfind(const char *json, jsmntok_t *tokens, int num_tokens, const char *name) {
    /* Assume the top-level element is an object */
    if (num_tokens < 1 || tokens[0].type != JSMN_OBJECT) {
      return 0;
    }
    /* Loop over all keys of the root object */
    for (int i = 1; i < num_tokens; i++) {
        if (jsoneq(json, &tokens[i], name)) {
            return i + 1;
        }
    }
    return 0;
}

bool jsonlookup(const char *json, jsmntok_t *tokens, int num_tokens, const char *name, char *value) {
    int token = jsonfind(json, tokens, num_tokens, name);

    if (token > 1) {
        memcpy(value, json+tokens[token].start, tokens[token].end - tokens[token].start);
        value[tokens[token].end - tokens[token].start] = '\0';
        return true;
    }
    return false;
}

void merge_json(char *dst, char *src, int *total_length) {
    src[0] = ' ';
    while (*src != '\0') {
        dst[*total_length]=*src++;
        *total_length += 1;
    }
    dst[*total_length-1] = ',';
}

{=# attrs =}
void set_attr_{= name =}_raw({= type =} unscaled_value) {
    attr_{= name =} = unscaled_value * {= scale =};
    {=# keep =}
    EEPROM.put({= addr =}, unscaled_value);
    {=/ keep =}
}

int set_attr_{= name =}(const char *json, jsmntok_t *tokens, int num_tokens, char *retval) {
    if (jsonlookup(json, tokens, num_tokens, "data", requestValue)) {
        {=# is_float =}
        {= type =} tmp = atof(requestValue);
        {=/ is_float =}
        {=^ is_float =}
        {= type =} tmp = atoi(requestValue);
        {=/ is_float =}
        if (tmp > {= max =} || tmp < {= min =}) {
          sprintf(retval, FC(F("{\"err\": \"data must between: [{= min =}, {= max =}]\"}")));
          return RET_ERR;
        }
        set_attr_{= name =}_raw(tmp);
    }
    get_attr_{= name =}(retval);
    return RET_SUCC;
}

int get_attr_{= name =}(char *retval) {
    {=^ is_float =}
    sprintf(retval, FC(F("{\"{= name =}\": %d}")), ({= type =})attr_{= name =} / {= scale =});
    {=/ is_float =}
    {=# is_float =}
    dtostrf(({= type =})attr_{= name =} / {= scale =}, {= width =}, {= prec =}, requestValue);
    sprintf(retval, FC(F("{\"{= name =}\": %s}")), ltrim(requestValue));
    {=/ is_float =}
    return RET_SUCC;
}

{=/ attrs =}
{=# metrics =}
int set_metric_{= name =}_threshold(const char *json, jsmntok_t *tokens, int num_tokens, char *retval) {
    if (jsonlookup(json, tokens, num_tokens, "data", requestValue)) {
        {= type =} tmp = atof(requestValue);
        if (tmp < {= min_threshold =} || tmp > {= max_threshold =}) {
          sprintf(retval, FC(F("{\"err\": \"data must between: [{= min_threshold =}, {= max_threshold =}]\"}")));
          return RET_ERR;
        }
        metric_{= name =}_threshold = tmp;
        EEPROM.put({= addr =}, metric_{= name =}_threshold);
    }
    get_metric_{= name =}_threshold(retval);
    return RET_SUCC;
}

int get_metric_{= name =}_threshold(char *retval) {
    dtostrf(metric_{= name =}_threshold, {= threshold_width =}, {= prec =}, requestValue);
    sprintf(retval, FC(F("{\"{= name =}_threshold\": %s}")), ltrim(requestValue));
    return RET_SUCC;
}

bool check_metric_{= name =}() {
    if (isnan(metric_{= name =})) {
        return false;
    }
    if (metric_{= name =} < {= min =}) {
        return false;
    }
    if (metric_{= name =} > {= max =}) {
        return false;
    }
    return true;
}

int invalid_metric_{= name =}_error(char *retval) {
    sprintf(retval, FC(F("{\"err\": \"{= name =} is invalid\"}")));
    return RET_ERR;
}

int get_metric_{= name =}(char *retval) {
    if (!check_metric_{= name =}()) {
        return invalid_metric_{= name =}_error(retval);
    }
    dtostrf(metric_{= name =}, {= width =}, {= prec =}, requestValue);
    sprintf(retval, FC(F("{\"{= name =}\": %s}")), ltrim(requestValue));
    return RET_SUCC;
}

{=/ metrics =}
{=/ has_app =}
{=# gpios =}
{=^ has_fn =}
{=# has_link =}
void open_gpio_{= name =}_raw() {
    gpio_{= name =}_state = {= open =};
    digitalWrite(gpio_{= name =}_pin, gpio_{= name =}_state);
}

void close_gpio_{= name =}_raw() {
    gpio_{= name =}_state = {= close =};
    digitalWrite(gpio_{= name =}_pin, gpio_{= name =}_state);
}

void open_gpio_{= name =}() {
    {=# reverse =}
    set_attr_{= link =}_raw({= close =});
    {=/ reverse =}
    {=^ reverse =}
    set_attr_{= link =}_raw({= open =});
    {=/ reverse =}
}

void close_gpio_{= name =}() {
    {=# reverse =}
    set_attr_{= link =}_raw({= open =});
    {=/ reverse =}
    {=^ reverse =}
    set_attr_{= link =}_raw({= close =});
    {=/ reverse =}
}
{=/ has_link =}
{=^ has_link =}
void open_gpio_{= name =}() {
    gpio_{= name =}_state = {= open =};
    digitalWrite(gpio_{= name =}_pin, gpio_{= name =}_state);
}

void close_gpio_{= name =}() {
    gpio_{= name =}_state = {= close =};
    digitalWrite(gpio_{= name =}_pin, gpio_{= name =}_state);
}
{=/ has_link =}

void toggle_gpio_{= name =}() {
    {=# has_link =}
    if (attr_{= link =} == {= open =}) {
        set_attr_{= link =}_raw({= close =});
    } else {
        set_attr_{= link =}_raw({= open =});
    }
    {=/ has_link =}
    {=^ has_link =}
    if (gpio_{= name =}_state == {= open =}) {
        close_gpio_{= name =}();
    } else {
        open_gpio_{= name =}();
    }
    {=/ has_link =}
}

{=/ has_fn =}
{=/ gpios =}
{=# functions =}
{=# has_argv =}
int {= name =}({= argv =}) {
{=/ has_argv =}
{=^ has_argv =}
{=# flag =}
{=# retval =}
{=# json =}
int {= name =}(const char *json, jsmntok_t *tokens, int num_tokens, char *retval) {
{=/ json =}
{=^ json =}
int {= name =}(char *retval) {
{=/ json =}
{=/ retval =}
{=^ retval =}
{=# json =}
int {= name =}(const char *json, jsmntok_t *tokens, int num_tokens) {
{=/ json =}
{=^ json =}
int {= name =}() {
{=/ json =}
{=/ retval =}
{=/ flag =}
{=/ has_argv =}
    {=& code =}
    {=^ return =}
    return RET_SUCC;
    {=/ return =}
}

{=/ functions =}
{=# has_app =}
int processRequest(const char *json, int length, char *retval) {
    /* Prepare parser */
    jsmn_init(&requestJsmnParser);
    int num_tokens = jsmn_parse(&requestJsmnParser, json, length, requestJsmnTokens, MAX_NUM_TOKENS);

    if (num_tokens < 0) {
        sprintf(retval, FC(F("{\"err\": \"Failed to parse JSON: %d\"}")), num_tokens);
        return RET_ERR;
    }

    int token = jsonfind(json, requestJsmnTokens, num_tokens, "method");

    if (token > 1) {
        {=# commands =}
        if (jsoneq(json, &requestJsmnTokens[token], FC(F("{= name =}")))) {
            {=# flag =}
            {=# retval =}
            {=# json =}
            int r = {= fn =}(json, requestJsmnTokens, num_tokens, retval);
            {=/ json =}
            {=^ json =}
            int r = {= fn =}(retval);
            {=/ json =}
            {=/ retval =}
            {=^ retval =}
            {=# json =}
            int r = {= fn =}(json, requestJsmnTokens, num_tokens);
            {=/ json =}
            {=^ json =}
            int r = {= fn =}();
            {=/ json =}
            {=/ retval =}
            if (r > RET_ERR) {
                return r;
            } else {
                {=# error =}
                sprintf(retval, FC(F("{\"err\": \"{= error =}\"}")));
                {=/ error =}
                return RET_ERR;
            }
            {=/ flag =}
        }
        {=/ commands =}
        {=# attrs =}
        {=# gen_set =}
        if (jsoneq(json, &requestJsmnTokens[token], FC(F("set_{= name =}")))) {
            int r = set_attr_{= name =}(json, requestJsmnTokens, num_tokens, retval);
            if (r > RET_ERR) {
                return r;
            } else {
                sprintf(retval, FC(F("{\"err\": \"set_{= name =} error\"}")));
                return RET_ERR;
            }
        }
        {=/ gen_set =}
        if (jsoneq(json, &requestJsmnTokens[token], FC(F("get_{= name =}")))) {
            int r = get_attr_{= name =}(retval);
            if (r > RET_ERR) {
                return r;
            } else {
                sprintf(retval, FC(F("{\"err\": \"get_{= name =} error\"}")));
                return RET_ERR;
            }
        }
        {=/ attrs =}
        {=# metrics =}
        if (jsoneq(json, &requestJsmnTokens[token], FC(F("set_{= name =}_threshold")))) {
            int r = set_metric_{= name =}_threshold(json, requestJsmnTokens, num_tokens, retval);
            if (r > RET_ERR) {
                return r;
            } else {
                sprintf(retval, FC(F("{\"err\": \"set_{= name =}_threshold error\"}")));
                return RET_ERR;
            }
        }
        if (jsoneq(json, &requestJsmnTokens[token], FC(F("get_{= name =}_threshold")))) {
            int r = get_metric_{= name =}_threshold(retval);
            if (r > RET_ERR) {
                return r;
            } else {
                sprintf(retval, FC(F("{\"err\": \"get_{= name =}_threshold error\"}")));
                return RET_ERR;
            }
        }
        if (jsoneq(json, &requestJsmnTokens[token], FC(F("get_{= name =}")))) {
            int r = get_metric_{= name =}(retval);
            if (r > RET_ERR) {
                return r;
            } else {
                sprintf(retval, FC(F("{\"err\": \"get_{= name =} error\"}")));
                return RET_ERR;
            }
        }
        {=/ metrics =}
    }
    return RET_ERR;
}

{=# has_metric =}
bool reportMetric(bool force) {
    bool wantSend = false;
    int total_length = 0;
    wantSendData[0] = '{';
    total_length += 1;

    {=# metrics =}
    if ((!isnan(metric_{= name =}) && metric_{= name =} >= {= min =} && metric_{= name =} <= {= max =} && abs(last_metric_{= name =} - metric_{= name =}) > metric_{= name =}_threshold) || force) {
        tempSendData[0] = '\0';
        if (get_metric_{= name =}(tempSendData) > RET_ERR) {
            merge_json(wantSendData, tempSendData, &total_length);
            wantSend = true;
            last_metric_{= name =} = metric_{= name =};
        }
    }

    {=/ metrics =}
    if (wantSend) {
        wantSendData[total_length-1] = '}';
        wantSendData[total_length] = '\0';

        send_packet_1(TELEMETRY, wantSendData);
        return true;
    }
    return false;
}

{=/ has_metric =}
{=# use_eeprom =}
bool reportAttribute(bool force) {
    int total_length = 0;
    bool wantSend = false;
    wantSendData[0] = '{';
    total_length += 1;

    {=# attrs =}
    if (last_attr_{= name =} != attr_{= name =} || force) {
        tempSendData[0] = '\0';
        if (get_attr_{= name =}(tempSendData) > RET_ERR) {
            merge_json(wantSendData, tempSendData, &total_length);
            wantSend = true;
            last_attr_{= name =} = attr_{= name =};
        }
    }

    {=/ attrs =}
    {=# metrics =}
    if (last_metric_{= name =}_threshold != metric_{= name =}_threshold || force) {
        tempSendData[0] = '\0';
        if (get_metric_{= name =}_threshold(tempSendData) > RET_ERR) {
            merge_json(wantSendData, tempSendData, &total_length);
            wantSend = true;
            last_metric_{= name =}_threshold = metric_{= name =}_threshold;
        }
    }

    {=/ metrics =}
    if (wantSend) {
        wantSendData[total_length-1] = '}';
        wantSendData[total_length] = '\0';

        send_packet_1(ATTRIBUTE, wantSendData);
        return true;
    }
    return false;
}
{=/ use_eeprom =}
{=/ has_app =}
