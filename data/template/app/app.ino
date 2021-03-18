{{={= =}=}}
{=# has_app =}
#include <givelink.h>
#include <jsmn.h>

{=/ has_app =}
{=# use_eeprom =}
#include <EEPROM.h>

{=/ use_eeprom =}
{=# has_uart =}
#include <SoftwareSerial.h>

{=/ has_uart =}
{=# consts =}
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
#define MAX_NUM_TOKENS 10
#endif

#ifndef MAX_REQUEST_VALUE_LENGTH
#define MAX_REQUEST_VALUE_LENGTH {= max_req_len =}
#endif

#ifndef MAX_TMPL_LENGTH
#define MAX_TMPL_LENGTH {= max_tpl_len =}
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
{=# use_eeprom =}
bool requireReportAttribute = true;
bool requireReportMetric = true;

{=/ use_eeprom =}
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
{= type =} metric_{= name =}_threshold = {= threshold =};
{= type =} last_metric_{= name =}_threshold = {= threshold =};
bool metric_{= name =}_threshold_force = true;
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
SoftwareSerial uart_{= name =}({= rx =}, {= tx =});
{=# readers =}
uint8_t uart_{= name =}_read_{= index =}_buffer[{= buf_len =}];
int uart_{= name =}_read_{= index =}_buffer_len = 0;
{=/ readers =}
{=# writers =}
bool is_uart_{= name =}_write_{= wname =} = false;
{=/ writers =}
bool uart_{= name =}_write_index = 0;
{=/ uarts =}
// defined
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
// -1 non num
//  0 int
//  1 float
int isnum(const char *buf);
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

    {=/ has_app =}
    {=# use_eeprom =}
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
    {=# has_app =}
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

    {=/ use_eeprom =}
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
    uart_{= name =}.begin({= speed =});
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
            if (rule_{= id =}_{= action =}_timer_ms + {= later =} < get_current_time_ms()) {
                {= action =}();
            }
            {=# has_else =}
            {=# has_else_later =}
            rule_{= id =}_{= else_action =}_timer_ms = get_current_time_ms();
            {=/ has_else_later =}
            {=/ has_else =}
        } else {
            rule_{= id =}_{= action =}_timer_ms = get_current_time_ms();
            {=# has_else =}
            {=# has_else_later =}
            if (rule_{= id =}_{= else_action =}_timer_ms + {= else_later =} < get_current_time_ms()) {
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
            {= action =}();
        {=# has_else =}
            {=# has_else_later =}
            rule_{= id =}_{= else_action =}_timer_ms = get_current_time_ms();
            {=/ has_else_later =}
        } else {
            {=# has_else_later =}
            if (rule_{= id =}_{= else_action =}_timer_ms + {= else_later =} < get_current_time_ms()) {
                {= else_action =}();
            }
            {=/ has_else_later =}
            {=^ has_else_later =}
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
                }
                if (obj.type == CTRLREQ) {
                    {=# ctrl_mode =}
                    mainAction();
                    {=/ ctrl_mode =}
                    send_packet_0(CTRLRES);
                }
                if (obj.type == CTRLREQ1) {
                    send_packet_0(PING);
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
        {=# use_eeprom =}
        requireReportAttribute = true;
        {=/ use_eeprom =}
        if (auth_timer_ms + AUTH_DELAY_MS < get_current_time_ms()) {
            send_packet_0(AUTHREQ);
            auth_timer_ms = get_current_time_ms();
        }
    {=^ ctrl_mode =}
    } else {
        mainAction();
    {=/ ctrl_mode =}
    }

    {=/ has_app =}
    {=# actions =}
    {=# has_on =}
    if ({= on =} && {= fn =}_timer_ms + {= delay_ms =} < get_current_time_ms()) {
    {=/ has_on =}
    {=^ has_on =}
    if ({= fn =}_timer_ms + {= delay_ms =} < get_current_time_ms()) {
    {=/ has_on =}
        {= fn =}();
        {= fn =}_timer_ms = get_current_time_ms();
    }
    {=/ actions =}
    {=# has_gpio =}
    {=# gpios =}
    {=# bind =}
    {=# is_fn =}
    gpio_reading = digitalRead({= pin =});
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
    while (uart_{= name =}.available() > 0) {
        {=# readers =}
        {=# has_on =}
        if ({= on =}) {
            if ({= reader =}(uart_{= name =}.read(), uart_{= name =}_read_{= index =}_buffer, &uart_{= name =}_read_{= index =}_buffer_len)) {
                {= parser =}(uart_{= name =}_read_{= index =}_buffer, uart_{= name =}_read_{= index =}_buffer_len);
                uart_{= name =}_read_{= index =}_buffer_len = 0;
            }
        }
        {=/ has_on =}
        {=^ has_on =}
        if ({= reader =}(uart_{= name =}.read(), uart_{= name =}_read_{= index =}_buffer, &uart_{= name =}_read_{= index =}_buffer_len)) {
            {= parser =}(uart_{= name =}_read_{= index =}_buffer, uart_{= name =}_read_{= index =}_buffer_len);
            uart_{= name =}_read_{= index =}_buffer_len = 0;
        }
        {=/ has_on =}
        {=/ readers =}
    }

    {=/ uarts =}
}

unsigned long get_current_time_ms() {
    return millis();
}

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
{=# has_app =}
void mainAction() {
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
}

void noop() {}

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
    {=/ low_memory =}
    {=^ low_memory =}
    givelink_to_binary(sendedPayload);
    {=/ low_memory =}
    uint16_t length = givelink_get_length();
    for (uint16_t i = 0; i < length; i ++) {
        {=# low_memory =}
        GL_SERIAL.write(readedPayload[i]);
        {=/ low_memory =}
        {=^ low_memory =}
        GL_SERIAL.write(sendedPayload[i]);
        {=/ low_memory =}
    }
    GL_SERIAL.write('\r');
    GL_SERIAL.write('\n');
    GL_SERIAL.flush();
}

void send_packet_0(const uint8_t type) {
    next_packet(type);
    send_packet();
}

void send_packet_1(const uint8_t type, const char *data) {
    next_packet(type);
    givelink_set_data((const uint8_t*)data, strlen(data));
    send_packet();
}

void send_packet_rsp(const char *data) {
    givelink_set_type(RESPONSE);
    givelink_set_data((const uint8_t*)wantSendData, strlen(data));
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
    sprintf(retval, FC(F("{\"{= name =}\": %ld}")), ({= type =})attr_{= name =} / {= scale =});
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
    }
    get_metric_{= name =}_threshold(retval);
    return true;
}

bool get_metric_{= name =}_threshold(char *retval) {
    {=^ is_float =}
    sprintf(retval, FC(F("{\"{= name =}_threshold\": %ld}")), metric_{= name =}_threshold);
    {=/ is_float =}
    {=# is_float =}
    dtostrf(metric_{= name =}_threshold, {= width =}, {= prec =}, requestValue);
    sprintf(retval, FC(F("{\"{= name =}_threshold\": %s}")), ltrim(requestValue));
    {=/ is_float =}
    return true;
}

{=/ auto =}
bool check_metric_{= name =}() {
    return is_valid_float(metric_{= name =}, {= min =}, {= max =});
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
    sprintf(retval, FC(F("{\"{= name =}\": %ld}")), metric_{= name =});
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
bool {= name =}({= argv =}) {
{=/ has_argv =}
{=^ has_argv =}
{=# flag =}
{=# retval =}
{=# json =}
bool {= name =}(const char *json, jsmntok_t *tokens, int num_tokens, char *retval) {
{=/ json =}
{=^ json =}
bool {= name =}(char *retval) {
{=/ json =}
{=/ retval =}
{=^ retval =}
{=# json =}
bool {= name =}(const char *json, jsmntok_t *tokens, int num_tokens) {
{=/ json =}
{=^ json =}
bool {= name =}() {
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
{=# writers =}
void uart_{= name =}_write_{= wname =}() {
    is_uart_{= name =}_write_{= wname =} = true;
    {=# bytes =}
    uart_{= name =}.write((uint8_t)0x{= . =});
    {=/ bytes =}
}

{=/ writers =}
void uart_{= name =}_write_next_index() {
    uart_{= name =}_write_index += 1;
    if (uart_{= name =}_write_index >= {= wcount =}) {
        uart_{= name =}_write_index = 0;
    }
}

void uart_{= name =}_write() {
    {=# writers =}
    is_uart_{= name =}_write_{= wname =} = false;
    {=/ writers =}
    {=# writers =}
    {=# has_on =}
    if ({= on =}) {
        if (uart_{= name =}_write_index == {= index =}) {
            uart_{= name =}_write_{= wname =}();
        }
    }
    {=/ has_on =}
    {=^ has_on =}
    if (uart_{= name =}_write_index == {= index =}) {
        uart_{= name =}_write_{= wname =}();
    }
    {=/ has_on =}
    {=/ writers =}
    uart_{= name =}_write_next_index();
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

        send_packet_1(ATTRIBUTE, wantSendData);
        return true;
    }
    return false;
}
{=/ use_eeprom =}
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
            send_packet_1(TELEMETRY, wantSendData);
            sended = true;
        }
    }

    {=/ auto =}
    {=/ metrics =}
    return sended;
}

{=/ has_metric =}
{=# use_eeprom =}
bool reportAttribute(bool force) {
    bool sended = true;
    {=# attrs =}
    if (last_attr_{= name =} != attr_{= name =} || force) {
        wantSendData[0] = '\0';
        if (get_attr_{= name =}(wantSendData)) {
            sended = true;
            last_attr_{= name =} = attr_{= name =};
            send_packet_1(ATTRIBUTE, wantSendData);
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
            send_packet_1(ATTRIBUTE, wantSendData);
        }
    }

    {=/ auto =}
    {=/ metrics =}
    return sended;
}
{=/ use_eeprom =}
{=/ low_memory =}
{=/ has_app =}
