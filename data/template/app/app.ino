{{={= =}=}}
{=# has_app =}
#include <givelink.h>
#include <jsmn.h>
#include <EEPROM.h>

{=/ has_app =}
{=# consts =}
{=^ has_type =}
#define {= name =} {= value =}
{=/ has_type =}
{=# has_type =}
{= type =} {= name =} = {= value =};
{=/ has_type =}
#define {= name =} {= value =}
{=/ consts =}
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

#ifndef SENDED_DELAY_MS
#define SENDED_DELAY_MS 100
#endif

unsigned long pong_timer_ms = 0;
unsigned long ping_timer_ms = 0;
bool ponged = true;
uint8_t ping_failed = 0;

#ifndef MAX_GL_PAYLOAD_LENGTH
#define MAX_GL_PAYLOAD_LENGTH {= max_gl_len =}
#endif
#ifndef MAX_BUFFER_LENGTH
#define MAX_BUFFER_LENGTH {= max_buf_len =}
#endif

#ifndef MAX_NUM_TOKENS
#define MAX_NUM_TOKENS 12
#endif

#ifndef MAX_REQUEST_VALUE_LENGTH
#define MAX_REQUEST_VALUE_LENGTH {= max_req_len =}
#endif

#ifndef MAX_TMPL_LENGTH
#define MAX_TMPL_LENGTH {= max_tpl_len =}
#endif

#ifdef ARDUINO_ARCH_RP2040
#define EEPROM_NEED_COMMIT
#ifndef EEPROM_SIZE
#define EEPROM_SIZE 1024
#endif
#endif

givelink_context_t ctx;
{=# app =}
uint8_t ctx_buff[{= context_len =}];
const uint8_t key[{= key_len =}] = {{= key_hex_array =}};
{=# production =}
uint8_t token[{= token_len =}];
uint8_t addr[{= addr_len =}];
{=/ production =}
{=^ production =}
uint8_t token[{= token_len =}] = {{= token_hex_array =}};
uint8_t addr[{= addr_len =}] = {{= addr_hex_array =}};
{=/ production =}
{=/ app =}

givelink_t obj;
uint8_t obj_buff[MAX_BUFFER_LENGTH];

bool crcFlag = false;
uint16_t lastPayloadId = 0;
uint16_t readedLen = 0;
uint8_t  readedPayload[MAX_GL_PAYLOAD_LENGTH];
{=^ low_memory =}
uint8_t  sendedPayload[MAX_GL_PAYLOAD_LENGTH];
{=# auto_retry =}
uint8_t  retryPayload[MAX_GL_PAYLOAD_LENGTH];
uint16_t retryLen = 0;
bool need_retry = false;
unsigned long retry_timer_ms = 0;
{=/ auto_retry =}
{=/ low_memory =}


jsmn_parser requestJsmnParser;
jsmntok_t requestJsmnTokens[MAX_NUM_TOKENS]; /* We expect no more than 128 tokens */
char requestValue[MAX_REQUEST_VALUE_LENGTH];
char wantSendData[MAX_BUFFER_LENGTH];
{=^ low_memory =}
char tempSendData[MAX_TMPL_LENGTH];
{=/ low_memory =}
char wantSendDataTpl[MAX_TMPL_LENGTH];

{=# has_metric =}
#ifndef METRIC_DELAY_MS
#define METRIC_DELAY_MS 1800000
#endif

unsigned long metric_timer_ms = 0;

{=/ has_metric =}
bool requireReportAttribute = true;
bool requireReportMetric = true;
{=/ has_app =}
{=# attrs =}
{= type =} attr_{= name =} = {= default =};
{=# has_app =}
{= type =} last_attr_{= name =} = {= default =};
bool attr_{= name =}_force = true;
{=/ has_app =}

{=/ attrs =}
{=# metrics =}
{= type =} metric_{= name =} = 0;
{=# has_app =}
{= type =} last_metric_{= name =} = 0;
{=# auto =}
{= type =} metric_{= name =}_threshold = {= threshold =};
{= type =} last_metric_{= name =}_threshold = {= threshold =};
bool metric_{= name =}_threshold_force = true;
{=/ auto =}
{=/ has_app =}

{=/ metrics =}
{=# actions =}
unsigned long {= fn =}_timer_ms = 0;
{=/ actions =}

{=# has_rule =}
bool rule_depends_checked = false;
{=# rules =}
{=# has_later =}
unsigned long rule_{= id =}_{= action =}_timer_ms = 0;
{=/ has_later =}
{=# has_else_later =}
unsigned long rule_{= id =}_{= else_action =}_timer_ms = 0;
{=/ has_else_later =}
bool rule_{= id =}_do_yes = true;
{=# has_else =}
bool rule_{= id =}_do_else  = true;
{=/ has_else =}
{=/ rules =}
{=/ has_rule =}
{=# has_input =}
#ifndef DEBOUNCE_DELAY_MS
#define DEBOUNCE_DELAY_MS 50
#endif
uint8_t gpio_reading = 0;
{=/ has_input =}
{=# has_gpio =}
{=# gpios =}
{=# bind =}
{=# is_link =}
uint8_t gpio_{= name =}_state = {= state =};
{=/ is_link =}
{=# is_fn =}
uint8_t gpio_{= name =}_state = {= state =};
unsigned long last_gpio_{= name =}_debounce_time_ms = 0;
uint8_t last_gpio_{= name =}_state = {= state =};
{=/ is_fn =}
{=# is_no_bind =}
uint8_t gpio_{= name =}_state = {= state =};
{=/ is_no_bind =}
{=# is_pwm =}
uint8_t gpio_{= name =}_state = {= state =};
{=/ is_pwm =}
{=/ bind =}
{=/ gpios =}
{=/ has_gpio =}
{=# agpios =}
{=# bind =}
{=# is_no_bind =}
uint16_t agpio_{= name =}_value = 0;
{=/ is_no_bind =}
{=/ bind =}
{=/ agpios =}
{=# uarts =}
bool is_{= name =}_readed = false;
{=# readers =}
uint8_t {= name =}_read_{= index =}_buffer[{= buf_len =}];
int {= name =}_read_{= index =}_buffer_len = 0;
{=/ readers =}
{=# writers =}
bool is_{= name =}_write_{= wname =} = false;
{=/ writers =}
uint8_t {= name =}_write_index = 0;
{=/ uarts =}
{=# has_timer =}
uint32_t sys_timer_s = 0;
uint32_t sys_timer_sync_ms = 0;
uint32_t next_timer_event_ms = 0;
uint32_t timer_delta0_ms = 0;
uint32_t timer_delta1_ms = 0;
uint32_t timer_schedat_s = 0;
uint32_t timer_period_s = 0;
uint32_t timer_duration_s = 0;
uint8_t timer_action = 0;
bool sys_timer_can_sync = true;
#ifndef SYNCTIME_DELAY_MS
#define SYNCTIME_DELAY_MS 3600000
#endif
{=/ has_timer =}
{=# timers =}
bool timer_{= name =}_sched = false;
{=/ timers =}
unsigned long current_time_ms = 0;
// defined
unsigned long get_current_time_ms();
unsigned long get_cache_time_ms();

{=# has_app =}
// -1 non num
//  0 int
//  1 float
int isnum(const char *buf);
{=# has_float =}
bool is_valid_float(float number, float min, float max);
{=/ has_float =}
void mainAction();
void noop();
void send_packet_raw(uint8_t * buf, uint16_t length);
void send_packet();
void send_packet_0(const uint8_t type);
void send_packet_1(const uint8_t type, const char *data, const int length);
void send_packet_rsp(const char *data);
void next_packet(const uint8_t type);
char * FC(const __FlashStringHelper *ifsh);
char * ltrim(char *s);
bool jsoneq(const char *json, jsmntok_t *token, const char *s);
int jsonfind(const char *json, jsmntok_t *tokens, int num_tokens, const char *name);

bool jsonlookup(const char *json, jsmntok_t *tokens, int num_tokens, const char *name, char *value);
{=^ low_memory =}
void merge_json(char *dst, char *src, int *total_length);
{=/ low_memory =}
int get_json_length(const char *src);
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
{= type =} {= name =}({= argv =});
{=/ has_argv =}
{=^ has_argv =}
{=# flag =}
{=# retval =}
{=# json =}
{= type =} {= name =}(const char *json, jsmntok_t *tokens, int num_tokens, char *retval);
{=/ json =}
{=^ json =}
{= type =} {= name =}(char *retval);
{=/ json =}
{=/ retval =}
{=^ retval =}
{=# json =}
{= type =} {= name =}(const char *json, jsmntok_t *tokens, int num_tokens);
{=/ json =}
{=^ json =}
{= type =} {= name =}();
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
bool reportAttribute(bool force);
{=/ has_app =}
{=# uarts =}
{=# writers =}
void {= name =}_write_{= wname =}();
{=/ writers =}
void {= name =}_write();
{=/ uarts =}
{=# has_timer =}
void swap_timer_event(uint32_t delta_ms);
uint32_t get_value(const char *json, jsmntok_t *tokens, int num_tokens, const char * name);
void set_timer_raw(const char *json, jsmntok_t *tokens, int num_tokens, int addr0, int addr1, int addr2);
bool get_timer_raw(int addr0, int addr1, int addr2, char *retval);
bool getset_timer(const char *json, jsmntok_t *tokens, int num_tokens, char *retval, bool set);
void processTimer(int addr0, int addr1, int addr2, bool * sched);

{=/ has_timer =}
// end defined
void setup() {
    {=# has_app =}
    {=# app =}
    givelink_context_init(&ctx, ctx_buff);
    givelink_context_set_key(key, {= key_len =});
    {=# production =}
    for (int i = 0; i < {= token_len =}; i ++) {
        token[i] = EEPROM.read({= token_addr =} + i);
    }
    {=/ production =}
    givelink_context_set_token(token, {= token_len =});
    {=# production =}
    for (int i = 0; i < {= addr_len =}; i ++) {
        addr[i] = EEPROM.read({= addr_addr =} + i);
    }
    givelink_context_set_addr(addr, {= addr_len =});
    givelink_context_set_auth(true);
    {=/ production =}
    givelink_init(&obj, obj_buff);
    {=/ app =}

    #ifdef EEPROM_SIZE
    EEPROM.begin(EEPROM_SIZE);
    #endif
    {=# attrs =}
    {=# keep =}
    {=# onebyte =}
    attr_{= name =} = EEPROM.read({= addr =});
    {=/ onebyte =}
    {=^ onebyte =}
    EEPROM.get({= addr =}, attr_{= name =});
    {=/ onebyte =}
    {=# is_float =}
    if (!is_valid_float(attr_{= name =}, {= scaled_min =}, {= scaled_max =})) {
    {=/ is_float =}
    {=^ is_float =}
    {=# uncheckmin =}
    if (attr_{= name =} > {= scaled_max =}) {
    {=/ uncheckmin =}
    {=^ uncheckmin =}
    if (attr_{= name =} < {= scaled_min =} || attr_{= name =} > {= scaled_max =}) {
    {=/ uncheckmin =}
    {=/ is_float =}
        attr_{= name =} = {= default =};
    }

    {=/ keep =}
    {=/ attrs =}
    {=# metrics =}
    {=# auto =}
    {=# onebyte =}
    metric_{= name =}_threshold = EEPROM.read({= addr =});
    {=/ onebyte =}
    {=^ onebyte =}
    EEPROM.get({= addr =}, metric_{= name =}_threshold);
    {=/ onebyte =}
    {=# is_float =}
    if (!is_valid_float(metric_{= name =}_threshold, {= min_threshold =}, {= max_threshold =})) {
    {=/ is_float =}
    {=^ is_float =}
    {=# uncheckmin =}
    if (metric_{= name =}_threshold > {= max_threshold =}) {
    {=/ uncheckmin =}
    {=^ uncheckmin =}
    if (metric_{= name =}_threshold < {= min_threshold =} || metric_{= name =}_threshold > {= max_threshold =}) {
    {=/ uncheckmin =}
    {=/ is_float =}
        metric_{= name =}_threshold = {= threshold =};
    }

    {=/ auto =}
    {=/ metrics =}

    {=/ has_app =}
    {=# has_gpio =}
    {=# gpios =}
    {=# bind =}
    {=# is_link =}
    pinMode({= pin =}, OUTPUT);
    digitalWrite({= pin =}, gpio_{= name =}_state);
    {=/ is_link =}
    {=# is_fn =}
    pinMode({= pin =}, INPUT);
    {=/ is_fn =}
    {=# is_pwm =}
    analogWrite({= pin =}, gpio_{= name =}_state);
    {=/ is_pwm =}
    {=# is_no_bind =}
    pinMode({= pin =}, OUTPUT);
    digitalWrite({= pin =}, gpio_{= name =}_state);
    {=/ is_no_bind =}
    {=/ bind =}
    {=/ gpios =}
    {=/ has_gpio =}
    {=# setups =}
    {=& code =}

    {=/ setups =}
    {=# uarts =}
    {= name =}.begin({= speed =});
    {=/ uarts =}
    {=# has_app =}
    {=# has_debug =}
    #ifdef DEBUG_SERIAL
    DEBUG_SERIAL.println(F("Setup"));
    #endif
    {=/ has_debug =}
    {=/ has_app =}
}

void loop() {
    current_time_ms = get_current_time_ms();
    {=# loops =}
    {=& code =}
    {=/ loops =}
    {=# rules =}
    rule_depends_checked = true;
    {=# depends =}
    if (!check_metric_{= . =}()) {
        rule_depends_checked = false;
    }
    {=/ depends =}
    {=# has_on =}
    if (rule_depends_checked && {=& on_condition =}) {
    {=/ has_on =}
    {=^ has_on =}
    if (rule_depends_checked) {
    {=/ has_on =}
        {=# has_later =}
        if ({=& condition =}) {
            if (rule_{= id =}_do_yes && rule_{= id =}_{= action =}_timer_ms + {= later =} < get_cache_time_ms()) {
                rule_{= id =}_do_yes = false;
                {= action =}();
            }
            {=# has_else =}
            rule_{= id =}_do_else = true;
            {=# has_else_later =}
            rule_{= id =}_{= else_action =}_timer_ms = get_cache_time_ms();
            {=/ has_else_later =}
            {=/ has_else =}
        } else {
            rule_{= id =}_do_yes = true;
            rule_{= id =}_{= action =}_timer_ms = get_cache_time_ms();
            {=# has_else =}
            {=# has_else_later =}
            if (rule_{= id =}_do_else && rule_{= id =}_{= else_action =}_timer_ms + {= else_later =} < get_cache_time_ms()) {
                rule_{= id =}_do_else = false;
                {= else_action =}();
            }
            {=/ has_else_later =}
            {=^ has_else_later =}
            {= else_action =}();
            {=/ has_else_later =}
            {=/ has_else =}
        }
        {=/ has_later =}
        {=^ has_later =}
        if ({=& condition =}) {
            if (rule_{= id =}_do_yes) {
                rule_{= id =}_do_yes = false;
                {= action =}();
            }
            {=# has_else =}
            rule_{= id =}_do_else = true;
            {=# has_else_later =}
            rule_{= id =}_{= else_action =}_timer_ms = get_cache_time_ms();
            {=/ has_else_later =}
            {=/ has_else =}
        } else {
            rule_{= id =}_do_yes = true;
            {=# has_else =}
            {=# has_else_later =}
            if (rule_{= id =}_do_else && rule_{= id =}_{= else_action =}_timer_ms + {= else_later =} < get_cache_time_ms()) {
                rule_{= id =}_do_else = false;
                {= else_action =}();
            }
            {=/ has_else_later =}
            {=^ has_else_later =}
            rule_{= id =}_do_else = false;
            {= else_action =}();
            {=/ has_else_later =}
            {=/ has_else =}
        }
        {=/ has_later =}
    }
    {=/ rules =}
    {=# has_app =}
    while (GL_SERIAL.available() > 0) {
        if (givelink_recv(readedPayload, &readedLen, GL_SERIAL.read(), &crcFlag)) {
            if (givelink_from_binary(readedPayload, readedLen)) {
                {=# has_debug =}
                #ifdef DEBUG_SERIAL
                DEBUG_SERIAL.print(F("Recv Id: "));
                DEBUG_SERIAL.print(obj.id);
                DEBUG_SERIAL.print(F(" Type: "));
                DEBUG_SERIAL.print(obj.type);
                if (obj.length > TYPE_LENGTH) {
                    DEBUG_SERIAL.print(F(" Data: "));
                    for (uint16_t i = 0; i < obj.length - TYPE_LENGTH; i ++) {
                        DEBUG_SERIAL.write(obj.data[i]);
                    }
                }
                DEBUG_SERIAL.println();
                #endif

                {=/ has_debug =}
                if (obj.type == AUTHRES && crcFlag) {
                    {=# production =}
                    {=# app =}
                    for (int i = 0; i < {= addr_len =}; i ++) {
                        EEPROM.write({= addr_addr =} + i, obj.data[i]);
                    }
                    #ifdef EEPROM_NEED_COMMIT
                    EEPROM.commit();
                    #endif
                    {=/ app =}
                    {=/ production =}
                }
                if (obj.type == REQUEST && crcFlag) {
                    wantSendData[0] = '\0';
                    bool ret = processRequest((const char *)obj.data, givelink_get_data_length(), wantSendData);
                    if (wantSendData[0] == '\0') {
                        if (ret) {
                            sprintf(wantSendData, FC(F("{\"result\": \"OK\"}")));
                        } else {
                            sprintf(wantSendData, FC(F("{\"err\": \"not support\"}")));
                        }
                    }
                    send_packet_rsp(wantSendData);
                }
                if (obj.type == SUCCESS) {
                    ponged = true;
                    {=^ low_memory =}
                    {=# auto_retry =}
                    need_retry = false;
                    {=/ auto_retry =}
                    {=/ low_memory =}
                }
                {=# has_timer =}
                if (obj.type == SYNCTIME) {
                    sys_timer_can_sync = true;
                    sys_timer_s = (uint32_t)atol((const char *)obj.data);
                }
                {=/ has_timer =}
                if (obj.type == CTRLREQ) {
                    {=# ctrl_mode =}
                    mainAction();
                    {=/ ctrl_mode =}
                    send_packet_0(CTRLRES);
                }
                if (obj.type == CTRLREQ1) {
                    {=^ low_memory =}
                    {=# auto_retry =}
                    if (!need_retry) {
                        send_packet_0(PING);
                    }
                    {=/ auto_retry =}
                    {=/ low_memory =}
                    {=# ctrl_mode =}
                    mainAction();
                    {=/ ctrl_mode =}
                    send_packet_0(CTRLRES);
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

    if (!givelink_context_authed()) {
        {=# has_app =}
        requireReportAttribute = true;
        {=/ has_app =}
        if (auth_timer_ms + AUTH_DELAY_MS < get_cache_time_ms()) {
            send_packet_0(AUTHREQ);
            auth_timer_ms = get_cache_time_ms();
        }
    {=^ ctrl_mode =}
    } else {
        mainAction();
    {=/ ctrl_mode =}
    }

    {=/ has_app =}
    {=# actions =}
    {=# has_on =}
    if ({= on =} && {= fn =}_timer_ms + {= delay_ms =} < get_cache_time_ms()) {
    {=/ has_on =}
    {=^ has_on =}
    if ({= fn =}_timer_ms + {= delay_ms =} < get_cache_time_ms()) {
    {=/ has_on =}
        {= fn =}();
        {= fn =}_timer_ms = get_cache_time_ms();
    }
    {=/ actions =}
    {=# has_gpio =}
    {=# gpios =}
    {=# bind =}
    {=# is_fn =}
    gpio_reading = digitalRead({= pin =});
    if (gpio_reading != last_gpio_{= name =}_state) {
        last_gpio_{= name =}_debounce_time_ms = get_cache_time_ms();
    }
    if ((get_cache_time_ms() - last_gpio_{= name =}_debounce_time_ms) > DEBOUNCE_DELAY_MS) {
        if (gpio_reading != gpio_{= name =}_state) {
          gpio_{= name =}_state = gpio_reading;
          if (gpio_{= name =}_state == {= emit =}) {
            {= fn =}();
          }
        }
    }
    last_gpio_{= name =}_state = gpio_reading;

    {=/ is_fn =}
    {=# is_link =}
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

    {=/ is_link =}
    {=# is_pwm =}
    if (attr_{= link =} != gpio_{= name =}_state) {
      gpio_{= name =}_state = attr_{= link =};
      analogWrite({= pin =}, gpio_{= name =}_state);
    }
    {=/ is_pwm =}
    {=/ bind =}
    {=/ gpios =}
    {=/ has_gpio =}
    {=# agpios =}
    {=# bind =}
    {=# is_link =}
    metric_{= link =} = analogRead({= pin =});
    {=/ is_link =}
    {=# is_no_bind =}
    agpio_{= name =}_value = analogRead({= pin =});
    {=/ is_no_bind =}
    {=/ bind =}
    {=/ agpios =}
    {=# uarts =}
    {= name =}_poll();
    {=/ uarts =}

    {=# has_timer =}
    if (sys_timer_s > 0 && next_timer_event_ms <= get_cache_time_ms()) {
        {=# timers =}
        processTimer({= addr0 =}, {= addr1 =}, {= addr2 =}, &timer_{= name =}_sched);
        if (timer_action == 1) {
            {= fn0 =}();
        } else if (timer_action == 2) {
            {= fn1 =}();
        }

        {=/ timers =}
        if (next_timer_event_ms < get_cache_time_ms()) {
            next_timer_event_ms += 60000;
        }
    }
    {=/ has_timer =}
}

unsigned long get_current_time_ms() {
    return millis();
}

unsigned long get_cache_time_ms() {
    return current_time_ms;
}

{=# has_app =}
// -1 non num
//  0 int
//  1 float
int isnum(const char *buf) {
    int length = strlen(buf);

    if (length == 0) {
        return -1;
    }

    if (!isdigit(buf[0])) {
        if (length == 1) {
            return -1;
        }
        if (buf[0] != '-') {
            return -1;
        }
    }

    if (buf[0] != '-' && !isdigit(buf[0])) {
        return -1;
    }

    int f = 0;

    for (int i = 1; i < length; i++) {
        if (!isdigit(buf[i])) {
            if (buf[i] == '.') {
                if (f == 1) {
                    return -1;
                } else {
                    f = 1;
                }
            } else {
                return -1;
            }
        }
    }

    return f;
}

{=# has_float =}
bool is_valid_float(float number, float min, float max) {
    if (isnan(number)) return false;
    if (isinf(number)) return false;
    if (number > max) return false;  // constant determined empirically
    if (number < min) return false;  // constant determined empirically
    return true;
}

{=/ has_float =}
void mainAction() {
    {=^ low_memory =}
    {=# auto_retry =}
    if (need_retry) {
        if (retry_timer_ms + 1000 < get_cache_time_ms()) {
            retry_timer_ms = get_cache_time_ms();
            send_packet_raw(retryPayload, retryLen);
        }
        return;
    }
    {=/ auto_retry =}
    {=/ low_memory =}
    {=# has_app =}
    reportAttribute(requireReportAttribute);
    if (requireReportAttribute) {
        requireReportAttribute = false;
    }
    {=# has_metric =}
    if (metric_timer_ms + METRIC_DELAY_MS < get_cache_time_ms()) {
        requireReportMetric = true;
    }
    if (reportMetric(requireReportMetric)) {
        metric_timer_ms = get_cache_time_ms();
        requireReportMetric = false;
    }
    {=/ has_metric =}
    {=/ has_app =}

    if (ping_timer_ms + PING_DELAY_MS < get_cache_time_ms()) {
        send_packet_0(PING);
        ponged = false;
        ping_timer_ms = get_cache_time_ms();
        pong_timer_ms = get_cache_time_ms();
    }

    if (ponged) {
        ping_failed = 0;
    } else {
        if (pong_timer_ms + PONG_DELAY_MS < get_cache_time_ms()) {
            ping_failed += 1;
            pong_timer_ms = get_cache_time_ms();

            if (ping_failed > MAX_PING_FAILED) {
                PING_FAILED_CB();
            }
        }
    }
    {=# has_timer =}
    if (sys_timer_can_sync) {
        if (sys_timer_s < 1000) {
            send_packet_0(SYNCTIME);
            sys_timer_can_sync = false;
        }

        if (sys_timer_sync_ms + SYNCTIME_DELAY_MS < get_cache_time_ms()) {
            send_packet_0(SYNCTIME);
            sys_timer_can_sync = false;
        }
    }
    {=/ has_timer =}
}

void noop() {}

void send_packet_raw(uint8_t * buf, uint16_t length) {
    delay(SENDED_DELAY_MS);
    for (uint16_t i = 0; i < length; i ++) {
        GL_SERIAL.write(buf[i]);
    }
    GL_SERIAL.write('\r');
    GL_SERIAL.write('\n');
    GL_SERIAL.flush();
}

void send_packet() {
    {=# has_debug =}
    #ifdef DEBUG_SERIAL
    DEBUG_SERIAL.print(F("Send Id: "));
    DEBUG_SERIAL.print(obj.id);
    DEBUG_SERIAL.print(F(" Type: "));
    DEBUG_SERIAL.print(obj.type);
    if (givelink_get_data_length() > 0) {
        DEBUG_SERIAL.print(F(" Data: "));
        for (uint16_t i = 0; i < givelink_get_data_length(); i ++) {
            DEBUG_SERIAL.write(obj.data[i]);
        }
    }
    DEBUG_SERIAL.println();
    #endif
    {=/ has_debug =}
    {=# low_memory =}
    givelink_to_binary(readedPayload);
    send_packet_raw(readedPayload, givelink_get_length());
    {=/ low_memory =}
    {=^ low_memory =}
    {=# auto_retry =}
    if (obj.type == ATTRIBUTE || obj.type == TELEMETRY) {
        givelink_to_binary(retryPayload);
        retryLen = givelink_get_length();
        need_retry = true;
        retry_timer_ms = get_cache_time_ms();

        send_packet_raw(retryPayload, retryLen);
    } else {
        givelink_to_binary(sendedPayload);
        send_packet_raw(sendedPayload, givelink_get_length());
    }
    {=/ auto_retry =}
    {=^ auto_retry =}
    givelink_to_binary(sendedPayload);
    send_packet_raw(sendedPayload, givelink_get_length());
    {=/ auto_retry =}
    {=/ low_memory =}
}

void send_packet_0(const uint8_t type) {
    next_packet(type);
    send_packet();
}

void send_packet_1(const uint8_t type, const char *data, const int length) {
    next_packet(type);
    givelink_set_data((const uint8_t*)data, length);
    send_packet();
}

void send_packet_rsp(const char *data) {
    givelink_set_type(RESPONSE);
    givelink_set_data((const uint8_t*)wantSendData, get_json_length(data));
    send_packet();
}

void next_packet(const uint8_t type) {
    givelink_reset();
    givelink_set_id(lastPayloadId);
    givelink_set_type(type);
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

{=^ low_memory =}
void merge_json(char *dst, char *src, int *total_length) {
    src[0] = ' ';
    while (*src != '}') {
        dst[*total_length]=*src++;
        *total_length += 1;
    }
    dst[*total_length] = ',';
    *total_length += 1;
}

{=/ low_memory =}
int get_json_length(const char *src) {
    int length = 1;
    while (*src != '}') {
        *src++;
        length += 1;
    }
    return length;
}

{=# attrs =}
void set_attr_{= name =}_raw({= type =} unscaled_value) {
    attr_{= name =}_force = true;
    attr_{= name =} = unscaled_value * {= scale =};
    {=# keep =}
    {=# onebyte =}
    EEPROM.write({= addr =}, attr_{= name =});
    {=/ onebyte =}
    {=^ onebyte =}
    EEPROM.put({= addr =}, attr_{= name =});
    {=/ onebyte =}
    {=/ keep =}
    #ifdef EEPROM_NEED_COMMIT
    EEPROM.commit();
    #endif
}

bool set_attr_{= name =}(const char *json, jsmntok_t *tokens, int num_tokens, char *retval) {
    if (jsonlookup(json, tokens, num_tokens, "data", requestValue)) {
        int tp = isnum(requestValue);
        if (tp == -1) {
            sprintf(retval, FC(F("{\"err\": \"data only support number\"}")));
            return false;
        }
        {=# is_float =}
        float tmp;
        if (tp == 0) {
            tmp = (float)atol(requestValue);
        } else {
            tmp = (float)atof(requestValue);
        }
        if (!is_valid_float(tmp, {= min =}, {= max =})) {
        {=/ is_float =}
        {=^ is_float =}
        long tmp;
        if (tp == 0) {
            tmp = (long)atol(requestValue);
        } else {
            tmp = (long)atof(requestValue);
        }
        if (tmp < {= min =} || tmp > {= max =}) {
        {=/ is_float =}
          sprintf(retval, FC(F("{\"err\": \"data must between: [{= min =}, {= max =}]\"}")));
          return false;
        }
        set_attr_{= name =}_raw(({= type =})tmp);
    }
    get_attr_{= name =}(retval);
    return true;
}

bool get_attr_{= name =}(char *retval) {
    {=^ is_float =}
    {=# is_long =}
    sprintf(retval, FC(F("{\"{= name =}\": %ld}")), ({= type =})attr_{= name =} / {= scale =});
    {=/ is_long =}
    {=^ is_long =}
    sprintf(retval, FC(F("{\"{= name =}\": %d}")), ({= type =})attr_{= name =} / {= scale =});
    {=/ is_long =}
    {=/ is_float =}
    {=# is_float =}
    dtostrf(({= type =})attr_{= name =} / {= scale =}, {= width =}, {= prec =}, requestValue);
    sprintf(retval, FC(F("{\"{= name =}\": %s}")), ltrim(requestValue));
    {=/ is_float =}
    return true;
}

{=/ attrs =}
{=# metrics =}
{=# auto =}
bool set_metric_{= name =}_threshold(const char *json, jsmntok_t *tokens, int num_tokens, char *retval) {
    if (jsonlookup(json, tokens, num_tokens, "data", requestValue)) {
        int tp = isnum(requestValue);
        if (tp == -1) {
            sprintf(retval, FC(F("{\"err\": \"data only support number\"}")));
            return false;
        }
        {=# is_float =}
        float tmp;
        if (tp == 0) {
            tmp = (float)atol(requestValue);
        } else {
            tmp = (float)atof(requestValue);
        }
        if (!is_valid_float(tmp, {= min_threshold =}, {= max_threshold =})) {
        {=/ is_float =}
        {=^ is_float =}
        long tmp;
        if (tp == 0) {
            tmp = (long)atol(requestValue);
        } else {
            tmp = (long)atof(requestValue);
        }
        if (tmp < {= min_threshold =} || tmp > {= max_threshold =}) {
        {=/ is_float =}
          sprintf(retval, FC(F("{\"err\": \"data must between: [{= min_threshold =}, {= max_threshold =}]\"}")));
          return false;
        }
        metric_{= name =}_threshold = ({= type =})tmp;
        metric_{= name =}_threshold_force = true;
        {=# onebyte =}
        EEPROM.write({= addr =}, metric_{= name =}_threshold);
        {=/ onebyte =}
        {=^ onebyte =}
        EEPROM.put({= addr =}, metric_{= name =}_threshold);
        {=/ onebyte =}
        #ifdef EEPROM_NEED_COMMIT
        EEPROM.commit();
        #endif
    }
    get_metric_{= name =}_threshold(retval);
    return true;
}

bool get_metric_{= name =}_threshold(char *retval) {
    {=^ is_float =}
    {=# is_long =}
    sprintf(retval, FC(F("{\"{= name =}_threshold\": %ld}")), metric_{= name =}_threshold);
    {=/ is_long =}
    {=^ is_long =}
    sprintf(retval, FC(F("{\"{= name =}_threshold\": %d}")), metric_{= name =}_threshold);
    {=/ is_long =}
    {=/ is_float =}
    {=# is_float =}
    dtostrf(metric_{= name =}_threshold, {= width =}, {= prec =}, requestValue);
    sprintf(retval, FC(F("{\"{= name =}_threshold\": %s}")), ltrim(requestValue));
    {=/ is_float =}
    return true;
}

{=/ auto =}
bool check_metric_{= name =}() {
    {=# is_float =}
    return is_valid_float(metric_{= name =}, {= min =}, {= max =});
    {=/ is_float =}
    {=^ is_float =}
    return metric_{= name =} >= {= min =} && metric_{= name =} <= {= max =};
    {=/ is_float =}
}

bool invalid_metric_{= name =}_error(char *retval) {
    sprintf(retval, FC(F("{\"err\": \"{= name =} is invalid\"}")));
    return false;
}

bool get_metric_{= name =}(char *retval) {
    if (!check_metric_{= name =}()) {
        return invalid_metric_{= name =}_error(retval);
    }
    {=^ is_float =}
    {=# is_long =}
    sprintf(retval, FC(F("{\"{= name =}\": %ld}")), metric_{= name =});
    {=/ is_long =}
    {=^ is_long =}
    sprintf(retval, FC(F("{\"{= name =}\": %d}")), metric_{= name =});
    {=/ is_long =}
    {=/ is_float =}
    {=# is_float =}
    dtostrf(metric_{= name =}, {= width =}, {= prec =}, requestValue);
    sprintf(retval, FC(F("{\"{= name =}\": %s}")), ltrim(requestValue));
    {=/ is_float =}
    return true;
}

{=/ metrics =}
{=/ has_app =}
{=# gpios =}
{=# bind =}
{=# is_link =}
void open_gpio_{= name =}_raw() {
    gpio_{= name =}_state = {= open =};
    digitalWrite({= pin =}, gpio_{= name =}_state);
}

void close_gpio_{= name =}_raw() {
    gpio_{= name =}_state = {= close =};
    digitalWrite({= pin =}, gpio_{= name =}_state);
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

void toggle_gpio_{= name =}() {
    if (attr_{= link =} == {= open =}) {
        set_attr_{= link =}_raw({= close =});
    } else {
        set_attr_{= link =}_raw({= open =});
    }
}

{=/ is_link =}
{=# is_no_bind =}
void open_gpio_{= name =}() {
    gpio_{= name =}_state = {= open =};
    digitalWrite({= pin =}, gpio_{= name =}_state);
}

void close_gpio_{= name =}() {
    gpio_{= name =}_state = {= close =};
    digitalWrite({= pin =}, gpio_{= name =}_state);
}

void toggle_gpio_{= name =}() {
    if (gpio_{= name =}_state == {= open =}) {
        close_gpio_{= name =}();
    } else {
        open_gpio_{= name =}();
    }
}

{=/ is_no_bind =}
{=/ bind =}
{=/ gpios =}
{=# functions =}
{=# has_argv =}
{= type =} {= name =}({= argv =}) {
{=/ has_argv =}
{=^ has_argv =}
{=# flag =}
{=# retval =}
{=# json =}
{= type =} {= name =}(const char *json, jsmntok_t *tokens, int num_tokens, char *retval) {
{=/ json =}
{=^ json =}
{= type =} {= name =}(char *retval) {
{=/ json =}
{=/ retval =}
{=^ retval =}
{=# json =}
{= type =} {= name =}(const char *json, jsmntok_t *tokens, int num_tokens) {
{=/ json =}
{=^ json =}
{= type =} {= name =}() {
{=/ json =}
{=/ retval =}
{=/ flag =}
{=/ has_argv =}
    {=& code =}
    {=^ return =}
    return true;
    {=/ return =}
}

{=/ functions =}
{=# uarts =}
void {= name =}_poll() {
    while ({= name =}.available() > 0) {
        uint8_t outByte = {= name =}.read();
        {=# readers =}
        {=# has_on =}
        if ({= on =}) {
            if ({= reader =}(outByte, {= name =}_read_{= index =}_buffer, &{= name =}_read_{= index =}_buffer_len)) {
                {= parser =}({= name =}_read_{= index =}_buffer, {= name =}_read_{= index =}_buffer_len);
                {= name =}_read_{= index =}_buffer_len = 0;
                is_{= name =}_readed = true;
            }
        }
        {=/ has_on =}
        {=^ has_on =}
        if ({= reader =}(outByte, {= name =}_read_{= index =}_buffer, &{= name =}_read_{= index =}_buffer_len)) {
            {= parser =}({= name =}_read_{= index =}_buffer, {= name =}_read_{= index =}_buffer_len);
            {= name =}_read_{= index =}_buffer_len = 0;
            is_{= name =}_readed = true;
        }
        {=/ has_on =}
        {=/ readers =}
    }
}

{=# writers =}
void {= name =}_write_{= wname =}() {
    is_{= name =}_write_{= wname =} = true;
    is_{= name =}_readed = false;
    {=# bytes =}
    {= name =}.write((uint8_t)0x{= . =});
    {=/ bytes =}
}

{=/ writers =}
bool {= name =}_is_valid_index() {
    {=# writers =}
    {=# auto =}
    {=# has_on =}
    if ({= on =}) {
        if ({= name =}_write_index == {= index =}) {
            return true;
        }
    }
    {=/ has_on =}
    {=^ has_on =}
    if ({= name =}_write_index == {= index =}) {
        return true;
    }
    {=/ has_on =}
    {=/ auto =}
    {=/ writers =}
    return false;
}

void {= name =}_write_next_index() {
    for (int i = 0; i < {= wcount =}; i ++) {
        {= name =}_write_index += 1;
        if ({= name =}_write_index >= {= wcount =}) {
            {= name =}_write_index = 0;
        }
        if ({= name =}_is_valid_index()) {
            break;
        }
    }
}

void {= name =}_write() {
    {=# writers =}
    {=# auto =}
    is_{= name =}_write_{= wname =} = false;
    {=/ auto =}
    {=/ writers =}
    {=# writers =}
    {=# auto =}
    {=# has_on =}
    if ({= on =}) {
        if ({= name =}_write_index == {= index =}) {
            {= name =}_write_{= wname =}();
        }
    }
    {=/ has_on =}
    {=^ has_on =}
    if ({= name =}_write_index == {= index =}) {
        {= name =}_write_{= wname =}();
    }
    {=/ has_on =}
    {=/ auto =}
    {=/ writers =}
    {= name =}_write_next_index();
}

{=/ uarts =}
{=# has_app =}
bool processRequest(const char *json, int length, char *retval) {
    /* Prepare parser */
    jsmn_init(&requestJsmnParser);
    int num_tokens = jsmn_parse(&requestJsmnParser, json, length, requestJsmnTokens, MAX_NUM_TOKENS);

    if (num_tokens < 0) {
        sprintf(retval, FC(F("{\"err\": \"Failed to parse JSON: %d\"}")), num_tokens);
        return false;
    }

    int token = jsonfind(json, requestJsmnTokens, num_tokens, "method");

    if (token > 1) {
        {=# commands =}
        if (jsoneq(json, &requestJsmnTokens[token], FC(F("{= name =}")))) {
            {=# flag =}
            {=# retval =}
            {=# json =}
            if ({= fn =}(json, requestJsmnTokens, num_tokens, retval)) {
            {=/ json =}
            {=^ json =}
            if ({= fn =}(retval)) {
            {=/ json =}
            {=/ retval =}
            {=^ retval =}
            {=# json =}
            if ({= fn =}(json, requestJsmnTokens, num_tokens)) {
            {=/ json =}
            {=^ json =}
            if ({= fn =}()) {
            {=/ json =}
            {=/ retval =}
                return true;
            } else {
                {=# error =}
                sprintf(retval, FC(F("{\"err\": \"{= error =}\"}")));
                {=/ error =}
                return false;
            }
            {=/ flag =}
        }
        {=/ commands =}
        {=# attrs =}
        {=# gen_set =}
        if (jsoneq(json, &requestJsmnTokens[token], FC(F("set_{= name =}")))) {
            return set_attr_{= name =}(json, requestJsmnTokens, num_tokens, retval);
        }
        {=/ gen_set =}
        if (jsoneq(json, &requestJsmnTokens[token], FC(F("get_{= name =}")))) {
            return get_attr_{= name =}(retval);
        }
        {=/ attrs =}
        {=# metrics =}
        {=# auto =}
        if (jsoneq(json, &requestJsmnTokens[token], FC(F("set_{= name =}_threshold")))) {
            return set_metric_{= name =}_threshold(json, requestJsmnTokens, num_tokens, retval);
        }
        if (jsoneq(json, &requestJsmnTokens[token], FC(F("get_{= name =}_threshold")))) {
            return get_metric_{= name =}_threshold(retval);
        }
        {=/ auto =}
        if (jsoneq(json, &requestJsmnTokens[token], FC(F("get_{= name =}")))) {
            return get_metric_{= name =}(retval);
        }
        {=/ metrics =}
        {=# has_timer =}
        if (jsoneq(json, &requestJsmnTokens[token], FC(F("set_timer")))) {
            return getset_timer(json, requestJsmnTokens, num_tokens, retval, true);
        }
        if (jsoneq(json, &requestJsmnTokens[token], FC(F("get_timer")))) {
            return getset_timer(json, requestJsmnTokens, num_tokens, retval, false);
        }
        {=/ has_timer =}
    }
    return false;
}

{=^ low_memory =}
{=# has_metric =}
bool reportMetric(bool force) {
    bool wantSend = false;
    int total_length = 0;
    wantSendData[0] = '{';
    total_length += 1;

    {=# metrics =}
    {=# auto =}
    if ((is_valid_float(metric_{= name =}, {= min =}, {= max =}) && abs(last_metric_{= name =} - metric_{= name =}) > metric_{= name =}_threshold) || force) {
        tempSendData[0] = '\0';
        if (get_metric_{= name =}(tempSendData)) {
            merge_json(wantSendData, tempSendData, &total_length);
            wantSend = true;
            last_metric_{= name =} = metric_{= name =};
        }
    }

    {=/ auto =}
    {=/ metrics =}
    if (wantSend) {
        wantSendData[total_length-1] = '}';
        wantSendData[total_length] = '\0';

        send_packet_1(TELEMETRY, wantSendData, total_length);
        return true;
    }
    return false;
}

{=/ has_metric =}
{=# has_app =}
bool reportAttribute(bool force) {
    int total_length = 0;
    bool wantSend = false;
    wantSendData[0] = '{';
    total_length += 1;

    {=# attrs =}
    if (last_attr_{= name =} != attr_{= name =} || force || attr_{= name =}_force) {
        tempSendData[0] = '\0';
        if (get_attr_{= name =}(tempSendData)) {
            merge_json(wantSendData, tempSendData, &total_length);
            wantSend = true;
            last_attr_{= name =} = attr_{= name =};
            attr_{= name =}_force = false;
        }
    }

    {=/ attrs =}
    {=# metrics =}
    {=# auto =}
    if (last_metric_{= name =}_threshold != metric_{= name =}_threshold || force || metric_{= name =}_threshold_force) {
        tempSendData[0] = '\0';
        if (get_metric_{= name =}_threshold(tempSendData)) {
            merge_json(wantSendData, tempSendData, &total_length);
            wantSend = true;
            last_metric_{= name =}_threshold = metric_{= name =}_threshold;
            metric_{= name =}_threshold_force = false;
        }
    }

    {=/ auto =}
    {=/ metrics =}
    if (wantSend) {
        wantSendData[total_length-1] = '}';
        wantSendData[total_length] = '\0';

        send_packet_1(ATTRIBUTE, wantSendData, total_length);
        return true;
    }
    return false;
}
{=/ has_app =}
{=/ low_memory =}
{=# low_memory =}
{=# has_metric =}
bool reportMetric(bool force) {
    bool sended = false;
    {=# metrics =}
    {=# auto =}
    if ((is_valid_float(metric_{= name =}, {= min =}, {= max =}) && abs(last_metric_{= name =} - metric_{= name =}) > metric_{= name =}_threshold) || force) {
        wantSendData[0] = '\0';
        if (get_metric_{= name =}(wantSendData)) {
            last_metric_{= name =} = metric_{= name =};
            send_packet_1(TELEMETRY, wantSendData, get_json_length(wantSendData));
            sended = true;
        }
    }

    {=/ auto =}
    {=/ metrics =}
    return sended;
}

{=/ has_metric =}
{=# has_app =}
bool reportAttribute(bool force) {
    bool sended = true;
    {=# attrs =}
    if (last_attr_{= name =} != attr_{= name =} || force) {
        wantSendData[0] = '\0';
        if (get_attr_{= name =}(wantSendData)) {
            sended = true;
            last_attr_{= name =} = attr_{= name =};
            send_packet_1(ATTRIBUTE, wantSendData, get_json_length(wantSendData));
        }
    }

    {=/ attrs =}
    {=# metrics =}
    {=# auto =}
    if (last_metric_{= name =}_threshold != metric_{= name =}_threshold || force) {
        wantSendData[0] = '\0';
        if (get_metric_{= name =}_threshold(wantSendData)) {
            sended = true;
            last_metric_{= name =}_threshold = metric_{= name =}_threshold;
            send_packet_1(ATTRIBUTE, wantSendData, get_json_length(wantSendData));
        }
    }

    {=/ auto =}
    {=/ metrics =}
    return sended;
}
{=/ has_app =}
{=/ low_memory =}
{=/ has_app =}

{=# has_timer =}
void swap_timer_event(uint32_t delta_ms) {
    if (delta_ms <= get_cache_time_ms()) {
        return;
    }
    if (next_timer_event_ms > get_cache_time_ms()) {
        if (next_timer_event_ms > delta_ms) {
            next_timer_event_ms = delta_ms;
        }
    } else {
        next_timer_event_ms = delta_ms;
    }
}

uint32_t get_value(const char *json, jsmntok_t *tokens, int num_tokens, const char * name) {
    if (jsonlookup(json, tokens, num_tokens, name, requestValue)) {
        int tp = isnum(requestValue);
        if (tp == -1) {
            return 0;
        }
        long tmp;
        if (tp == 0) {
            tmp = (long)atol(requestValue);
        } else {
            tmp = (long)atof(requestValue);
        }

        if (tmp > 0) {
            return (uint32_t) tmp;
        }
    }
    return 0;
}

void set_timer_raw(const char *json, jsmntok_t *tokens, int num_tokens, int addr0, int addr1, int addr2) {
    timer_schedat_s  = get_value(json, tokens, num_tokens, "sched_at");
    timer_period_s   = get_value(json, tokens, num_tokens, "period");
    timer_duration_s = get_value(json, tokens, num_tokens, "duration");
    EEPROM.put(addr0, timer_schedat_s);
    EEPROM.put(addr1, timer_period_s);
    EEPROM.put(addr2, timer_duration_s);
    #ifdef EEPROM_NEED_COMMIT
    EEPROM.commit();
    #endif
    timer_delta0_ms = (timer_schedat_s - sys_timer_s) * 1000 + sys_timer_sync_ms;
    swap_timer_event(timer_delta0_ms);
}

bool get_timer_raw(int addr0, int addr1, int addr2, char *retval) {
    EEPROM.get(addr0, timer_schedat_s);
    EEPROM.get(addr1, timer_period_s);
    EEPROM.get(addr2, timer_duration_s);
    sprintf(retval, FC(F("{\"sched_at\": %ld, \"period\": %ld, \"duration\": %ld}")), timer_schedat_s, timer_period_s, timer_duration_s);
    return true;
}

bool getset_timer(const char *json, jsmntok_t *tokens, int num_tokens, char *retval, bool set) {
    int token = jsonfind(json, tokens, num_tokens, "name");
    if (token > 1) {
        {=# timers =}
        if (jsoneq(json, &tokens[token], FC(F("{= name =}")))) {
            if (set) {
                set_timer_raw(json, tokens, num_tokens, {= addr0 =}, {= addr1 =}, {= addr2 =});
            }
            return get_timer_raw({= addr0 =}, {= addr1 =}, {= addr2 =}, retval);
        }
        {=/ timers =}
    }
    return false;
}

void finishTimer(int addr0, int addr1, bool *sched) {
    if (*sched) {
        timer_delta1_ms = (timer_schedat_s - sys_timer_s + timer_duration_s) * 1000 + sys_timer_sync_ms;
        if (timer_delta1_ms <= get_cache_time_ms()) {
            *sched = false;
            timer_action = 2;
            EEPROM.get(addr1, timer_period_s);
            if (timer_period_s > 0) {
                timer_schedat_s += timer_period_s;
                EEPROM.put(addr0, timer_schedat_s);
                #ifdef EEPROM_NEED_COMMIT
                EEPROM.commit();
                #endif
                timer_delta0_ms = (timer_schedat_s - sys_timer_s) * 1000 + sys_timer_sync_ms;
            }
        }
    }
}

void processTimer(int addr0, int addr1, int addr2, bool * sched) {
    processTimer0(addr0, addr1, addr2, sched);
    swap_timer_event(timer_delta0_ms);
    swap_timer_event(timer_delta1_ms);
}

void processTimer0(int addr0, int addr1, int addr2, bool * sched) {
    timer_action = 0;
    EEPROM.get(addr0, timer_schedat_s);
    EEPROM.get(addr2, timer_duration_s);

    if (timer_duration_s == 0) {
        return;
    }

    if (timer_schedat_s < sys_timer_s || *sched) {
        finishTimer(addr0, addr1, sched);
        return;
    }

    timer_delta0_ms = (timer_schedat_s - sys_timer_s) * 1000 + sys_timer_sync_ms;
    timer_delta1_ms = timer_duration_s * 1000 + timer_delta0_ms;

    if (timer_delta0_ms <= get_cache_time_ms() && timer_delta1_ms > get_cache_time_ms()) {
        *sched = true;
        timer_action = 1;
    }
}

{=/ has_timer =}
