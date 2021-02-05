{{={= =}=}}
{=# use_remote =}
#include <avr/wdt.h>
#include <givelink.h>
#include <jsmn.h>

{=/ use_remote =}
{=# use_eeprom =}
#include <EEPROM.h>

{=/ use_eeprom =}
{=# has_func =}
#define RET_ERR -1
#define RET_SUCC 0

{=/ has_func =}
unsigned long get_current_time_ms();

{=# inits =}
{=& code =}

{=/ inits =}
{=# use_remote =}
unsigned long auth_timer_ms = get_current_time_ms();

#ifndef AUTH_DELAY_MS
#define AUTH_DELAY_MS 1000
#endif

#ifndef PONG_DELAY_MS
#define PONG_DELAY_MS 6000
#endif

#ifndef PING_DELAY_MS
#define PING_DELAY_MS 300000
#endif

#ifndef MAX_PING_FAILED
#define MAX_PING_FAILED 10
#endif

unsigned long pong_timer_ms = get_current_time_ms();
unsigned long ping_timer_ms = get_current_time_ms();
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

uint16_t lastPayloadId = 0;
uint16_t readedLen = 0;
uint8_t  readedPayload[MAX_GL_PAYLOAD_LENGTH + 1];
uint8_t  sendedPayload[MAX_GL_PAYLOAD_LENGTH + 1];


jsmn_parser requestJsmnParser;
jsmntok_t requestJsmnTokens[MAX_NUM_TOKENS]; /* We expect no more than 128 tokens */
char requestValue[MAX_REQUEST_VALUE_LENGTH];
givelink_t * m = givelink_new(MAX_GL_PAYLOAD_LENGTH);
char wantSendData[WANT_SEND_DATA_LENGTH];
char tempSendData[WANT_SEND_DATA_LENGTH];
char * wantSendDataTpl = (char *)malloc(WANT_SEND_DATA_LENGTH);

{=/ use_remote =}
{=# has_metric =}
#ifndef METRIC_DELAY_MS
#define METRIC_DELAY_MS 1800000
#endif

unsigned long metric_timer_ms = get_current_time_ms();

{=/ has_metric =}
{=# use_eeprom =}
bool requireReportAttribute = false;

{=/ use_eeprom =}
{=# attrs =}
{= type =} {= var =} = {= default =};
{= type =} last_{= var =} = {= default =};

{=/ attrs =}
{=# metrics =}
{= type =} {= var =} = 0;
{= type =} last_{= var =} = 0;
{= type =} {= var =}_threshold = {= threshold =};
{= type =} last_{= var =}_threshold = {= threshold =};

{=/ metrics =}
{=# actions =}
unsigned long {= fn =}_timer_ms = get_current_time_ms();
{=/ actions =}

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
unsigned long last_gpio_{= name =}_debounce_time_ms = get_current_time_ms();
int last_gpio_{= name =}_state = {= state =};
{=/ has_fn =}

{=/ gpios =}
{=/ has_gpio =}
void setup() {
    {=# use_remote =}
    // wdt init
    MCUSR = 0;
    wdt_disable();
    wdt_enable(WDTO_8S);
    // end wdt init
    {=# app =}
    givelink_init("{= key =}", "{= token =}");
    {=/ app =}

    {=/ use_remote =}
    {=# use_eeprom =}
    byte first_run_flag = 0;
    EEPROM.get(0, first_run_flag);

    {=# attrs =}
    if (first_run_flag == 1) {
        EEPROM.get({= addr =}, {= var =});
        if ({= var =} > {= scaled_max =} || {= var =} < {= scaled_min =}) {
            {= var =} = {= default =};
        }
    } else {
        EEPROM.put({= addr =}, {= var =});
    }

    {=/ attrs =}
    {=# metrics =}
    if (first_run_flag == 1) {
        EEPROM.get({= addr =}, {= var =}_threshold);
        if ({= var =}_threshold > {= max_threshold =} || {= var =}_threshold < {= min_threshold =}) {
            {= var =}_threshold = {= threshold =};
        }
    } else {
        EEPROM.put({= addr =}, {= var =}_threshold);
    }

    {=/ metrics =}
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
    {=# use_remote =}
    {=# has_debug =}
    #ifdef DEBUG_SERIAL
    DEBUG_SERIAL.println(F("Setup"));
    #endif
    {=/ has_debug =}
    {=/ use_remote =}
}

void loop() {
    {=# use_remote =}
    wdt_reset();
    {=/ use_remote =}
    {=# loops =}
    {=& code =}
    {=/ loops =}
    {=# use_remote =}
    while (GL_SERIAL.available() > 0) {
        uint8_t outByte = GL_SERIAL.read();
        if (givelink_recv(readedPayload, &readedLen, outByte)) {
            if (givelink_from_binary(m, readedPayload, readedLen)) {
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

    if (givelink_authed()) {
        {=# use_eeprom =}
        reportAttribute(requireReportAttribute);
        if (requireReportAttribute) {
            requireReportAttribute = false;
        }
        {=/ use_eeprom =}
        {=# has_metric =}
        if (metric_timer_ms + METRIC_DELAY_MS < get_current_time_ms() || checkValue()) {
            metric_timer_ms = get_current_time_ms();
            processTelemetries();
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
                    reset();
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

    {=/ use_remote =}
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
    if ({= link =} == gpio_{= name =}_state) {
        if ({= link =} == {= open =}) {
            close_{= name =}();
        } else {
            open_{= name =}();
        }
    }
    {=/ reverse =}
    {=^ reverse =}
    if ({= link =} != gpio_{= name =}_state) {
        if ({= link =} == {= open =}) {
            open_{= name =}();
        } else {
            close_{= name =}();
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

{=# use_remote =}
void reset() {
    wdt_disable();
    wdt_enable(WDTO_15MS);
    for (;;) {

    }
}

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
    givelink_to_binary(m, sendedPayload);
    uint16_t length = givelink_get_length(m);
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

{=/ use_remote =}
{=# attrs =}
{=# gen_set =}
int set_{= var =}(const char *json, jsmntok_t *tokens, int num_tokens, char *retval) {
    if (jsonlookup(json, tokens, num_tokens, "data", requestValue)) {
        {= type =} tmp = atoi(requestValue);
        if (tmp > {= max =} || tmp < {= min =}) {
          sprintf(retval, FC(F("{\"err\": \"data must between: [{= min =}, {= max =}]\"}")));
          return RET_ERR;
        }
        {= var =} = tmp * {= scale =};
        EEPROM.put({= addr =}, tmp);
    }
    get_{= var =}(retval);
    return RET_SUCC;
}

{=/ gen_set =}
int get_{= var =}(char *retval) {
    sprintf(retval, FC(F("{\"{= name =}\": %d}")), ({= type =}){= var =} / {= scale =});
    return RET_SUCC;
}

{=/ attrs =}
{=# metrics =}
int set_{= var =}_threshold(const char *json, jsmntok_t *tokens, int num_tokens, char *retval) {
    if (jsonlookup(json, tokens, num_tokens, "data", requestValue)) {
        {= type =} tmp = atof(requestValue);
        if (tmp < {= min_threshold =} || tmp > {= max_threshold =}) {
          sprintf(retval, FC(F("{\"err\": \"data must between: [{= min_threshold =}, {= max_threshold =}]\"}")));
          return RET_ERR;
        }
        {= var =}_threshold = tmp;
        EEPROM.put({= addr =}, {= var =}_threshold);
    }
    get_{= var =}_threshold(retval);
    return RET_SUCC;
}

int get_{= var =}_threshold(char *retval) {
    dtostrf({= var =}_threshold, {= threshold_width =}, {= prec =}, requestValue);
    sprintf(retval, FC(F("{\"{= name =}_threshold\": %s}")), ltrim(requestValue));
    return RET_SUCC;
}

int invalid_{= var =}_error(char *retval) {
    sprintf(retval, FC(F("{\"err\": \"{= name =} is invalid\"}")));
    return RET_ERR;
}

int get_{= var =}(char *retval) {
    if (isnan({= var =})) {
        return invalid_{= var =}_error(retval);
    }
    if ({= var =} < {= min =}) {
        return invalid_{= var =}_error(retval);
    }
    if ({= var =} > {= max =}) {
        return invalid_{= var =}_error(retval);
    }
    dtostrf({= var =}, {= width =}, {= prec =}, requestValue);
    sprintf(retval, FC(F("{\"{= name =}\": %s}")), ltrim(requestValue));
    return RET_SUCC;
}

{=/ metrics =}
{=# gpios =}
{=^ has_fn =}
void open_{= name =}() {
    gpio_{= name =}_state = {= open =};
    digitalWrite(gpio_{= name =}_pin, gpio_{= name =}_state);
}

void close_{= name =}() {
    gpio_{= name =}_state = {= close =};
    digitalWrite(gpio_{= name =}_pin, gpio_{= name =}_state);
}

void toggle_{= name =}() {
    {=# has_link =}
    if ({= link =} == {= open =}) {
        {= link =} = {= close =};
    } else {
        {= link =} = {= open =};
    }
    {=/ has_link =}
    {=^ has_link =}
    if (gpio_{= name =}_state == {= open =}) {
        close_{= name =}();
    } else {
        open_{= name =}();
    }
    {=/ has_link =}
}

{=/ has_fn =}
{=/ gpios =}
{=# functions =}
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
    {=& code =}
    {=^ return =}
    return RET_SUCC;
    {=/ return =}
}

{=/ functions =}
{=# use_remote =}
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
            int r = set_{= var =}(json, requestJsmnTokens, num_tokens, retval);
            if (r > RET_ERR) {
                return r;
            } else {
                sprintf(retval, FC(F("{\"err\": \"set_{= name =} error\"}")));
                return RET_ERR;
            }
        }
        {=/ gen_set =}
        if (jsoneq(json, &requestJsmnTokens[token], FC(F("get_{= name =}")))) {
            int r = get_{= var =}(retval);
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
            int r = set_{= var =}_threshold(json, requestJsmnTokens, num_tokens, retval);
            if (r > RET_ERR) {
                return r;
            } else {
                sprintf(retval, FC(F("{\"err\": \"set_{= name =}_threshold error\"}")));
                return RET_ERR;
            }
        }
        if (jsoneq(json, &requestJsmnTokens[token], FC(F("get_{= name =}_threshold")))) {
            int r = get_{= var =}_threshold(retval);
            if (r > RET_ERR) {
                return r;
            } else {
                sprintf(retval, FC(F("{\"err\": \"get_{= name =}_threshold error\"}")));
                return RET_ERR;
            }
        }
        if (jsoneq(json, &requestJsmnTokens[token], FC(F("get_{= name =}")))) {
            int r = get_{= var =}(retval);
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

{=/ use_remote =}
{=# has_metric =}
bool processTelemetries() {
    bool ret = false;
    bool wantSend = false;
    int total_length = 0;
    wantSendData[0] = '{';
    total_length += 1;

    {=# telemetries =}
    tempSendData[0] = '\0';
    {=# flag =}
    {=# retval =}
    {=# json =}
    if ({= fn =}(NULL, NULL, 0, tempSendData) > RET_ERR) {
    {=/ json =}
    {=^ json =}
    if ({= fn =}(tempSendData) > RET_ERR) {
    {=/ json =}
        merge_json(wantSendData, tempSendData, &total_length);
        wantSend = true;
    }
    {=/ retval =}
    {=/ flag =}

    {=/ telemetries =}
    {=# metrics =}
    tempSendData[0] = '\0';
    if (get_{= var =}(tempSendData) > RET_ERR) {
        merge_json(wantSendData, tempSendData, &total_length);
        wantSend = true;
        last_{= var =} = {= var =};
    }

    {=/ metrics =}
    if (wantSend) {
        wantSendData[total_length-1] = '}';
        wantSendData[total_length] = '\0';

        send_packet_1(TELEMETRY, wantSendData);
        ret = true;
    }
    return ret;
}

bool checkValue() {
    {=# metrics =}
    if (!isnan({= var =}) && {= var =} >= {= min =} && {= var =} <= {= max =} && abs(last_{= var =} - {= var =}) > {= var =}_threshold) {
        return true;
    }

    {=/ metrics =}
    return false;
}

{=/ has_metric =}
{=# use_eeprom =}
bool reportAttribute(bool force) {
    int total_length = 0;
    bool report = true;
    bool wantSend = false;
    wantSendData[0] = '{';
    total_length += 1;

    {=# attrs =}
    report = last_{= var =} != {= var =};
    if (force) {
        report = true;
    }
    if (report) {
        tempSendData[0] = '\0';
        if (get_{= var =}(tempSendData) > RET_ERR) {
            merge_json(wantSendData, tempSendData, &total_length);
            wantSend = true;
            last_{= var =} = {= var =};
        }
    }

    {=/ attrs =}
    {=# metrics =}
    report = last_{= var =}_threshold != {= var =}_threshold;
    if (force) {
        report = true;
    }
    if (report) {
        tempSendData[0] = '\0';
        if (get_{= var =}_threshold(tempSendData) > RET_ERR) {
            merge_json(wantSendData, tempSendData, &total_length);
            wantSend = true;
            last_{= var =}_threshold = {= var =}_threshold;
        }
    }

    {=/ metrics =}
    if (wantSend) {
        wantSendData[total_length-1] = '}';
        wantSendData[total_length] = '\0';

        send_packet_1(ATTRIBUTE, wantSendData);
    }
    return RET_SUCC;
}
{=/ use_eeprom =}
