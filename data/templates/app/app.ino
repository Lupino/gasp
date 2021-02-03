{{={= =}=}}
#include <avr/wdt.h>
#include <givelink.h>
#include <jsmn.h>
{=# use_eeprom =}
#include <EEPROM.h>
{=/ use_eeprom =}

{=# inits =}
{=& code =}

{=/ inits =}

unsigned long get_current_time_ms();

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

unsigned long ping_timer_ms = get_current_time_ms();
bool ponged = true;
int ping_failed = 0;
{=# has_metric =}

#ifndef METRIC_DELAY_MS
#define METRIC_DELAY_MS 1800000
#endif

unsigned long metric_timer_ms = get_current_time_ms();
{=/ has_metric =}

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
{=# use_eeprom =}

bool requireReportAttribute = false;
{=/ use_eeprom =}

givelink_t * m = givelink_new(MAX_GL_PAYLOAD_LENGTH);

char wantSendData[WANT_SEND_DATA_LENGTH];
char tempSendData[WANT_SEND_DATA_LENGTH];
char * wantSendDataTpl = (char *)malloc(WANT_SEND_DATA_LENGTH);

#define RET_ERR -1
#define RET_SUCC 0
#define RET_ATTR 1

{=# attrs =}
{= type =} {= var =} = {= default =};

{=/ attrs =}
{=# metrics =}
{= type =} {= var =} = 0;
{= type =} last_{= var =} = 0;
{= type =} {= var =}_threshold = {= threshold =};

{=/ metrics =}
{=# monitors =}
unsigned long monitor_{= name =}_timer_ms = get_current_time_ms();
{=/ monitors =}

void setup() {
    // wdt init
    MCUSR = 0;
    wdt_disable();
    wdt_enable(WDTO_8S);
    // end wdt init
    {=# app =}
    givelink_init("{= key =}", "{= token =}");
    {=/ app =}

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
    {=# setups =}
    {=& code =}

    {=/ setups =}
    #ifdef DEBUG
    DEBUG_SERIAL.println(F("Setup"));
    #endif
}

void loop() {
    wdt_reset();
    {=# loops =}
    {=& code =}
    {=/ loops =}
    while (GL_SERIAL.available() > 0) {
        uint8_t outByte = GL_SERIAL.read();
        if (givelink_recv(readedPayload, &readedLen, outByte)) {
            if (givelink_from_binary(m, readedPayload, readedLen)) {
                #ifdef DEBUG
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

                    if (ret == RET_ATTR) {
                        send_packet_1(ATTRIBUTE, wantSendData);
                    }
                }
                if (m -> type == SUCCESS) {
                    ponged = true;
                }
            }
            readedLen = 0;
        }
        if (readedLen > MAX_GL_PAYLOAD_LENGTH) {
            #ifdef DEBUG
            DEBUG_SERIAL.println(F("Error: payload to large"));
            #endif
            readedLen = 0;
        }
    }

    if (givelink_authed()) {
        {=# use_eeprom =}
        if (requireReportAttribute) {
            requireReportAttribute = false;
            reportAttribute();
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
        }

        if (ponged) {
            ping_failed = 0;
        } else {
            if (ping_timer_ms + PONG_DELAY_MS < get_current_time_ms()) {
                ping_failed += 1;

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

    {=# monitors =}
    if (monitor_{= name =}_timer_ms + {= delay_ms =} < get_current_time_ms()) {
        {= fn =}();
        monitor_{= name =}_timer_ms = get_current_time_ms();
    }
    {=/ monitors =}
}

void send_packet() {
    #ifdef DEBUG
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

unsigned long get_current_time_ms() {
    return millis();
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

{=# attrs =}
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
    return RET_ATTR;
}
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
    return RET_ATTR;
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
    last_{= var =} = {= var =};
    sprintf(retval, FC(F("{\"{= name =}\": %s}")), ltrim(requestValue));
    return RET_SUCC;
}

{=/ metrics =}
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
}
{=/ functions =}

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
            if (r > RET_ERR) {
                return r;
            } else {
                {=# error =}
                sprintf(retval, FC(F("{\"err\": \"{= error =}\"}")));
                {=/ error =}
                return RET_ERR;
            }
            {=/ retval =}
            {=/ flag =}
        }
        {=/ commands =}
        {=# attrs =}
        if (jsoneq(json, &requestJsmnTokens[token], FC(F("set_{= name =}")))) {
            int r = set_{= var =}(json, requestJsmnTokens, num_tokens, retval);
            if (r > RET_ERR) {
                return r;
            } else {
                sprintf(retval, FC(F("{\"err\": \"set_{= name =} error\"}")));
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

{=# has_metric =}
bool processTelemetries() {
    bool ret = false;
    {=# telemetries =}
    wantSendData[0] = '\0';
    {=# flag =}
    {=# retval =}
    {=# json =}
    if ({= fn =}(NULL, NULL, 0, wantSendData) > RET_ERR) {
    {=/ json =}
    {=^ json =}
    if ({= fn =}(wantSendData) > RET_ERR) {
    {=/ json =}
      send_packet_1(TELEMETRY, wantSendData);
      ret = true;
    }
    {=/ retval =}
    {=/ flag =}

    {=/ telemetries =}
    bool wantSend = false;
    size_t total_length = 0;
    size_t length = 0;
    size_t i = 0;
    wantSendData[0] = '{';
    total_length += 1;
    {=# metrics =}
    if (!isnan({= var =}) && {= var =} >= {= min =} && {= var =} <= {= max =}) {
        wantSend = true;
        requestValue[0] = '\0';
        dtostrf({= var =}, {= width =}, {= prec =}, requestValue);
        sprintf(tempSendData, FC(F("\"{= name =}\": %s,")), ltrim(requestValue));
        length = strlen(tempSendData);
        for (i=0; i<length; i++) {
            wantSendData[total_length + i] = tempSendData[i];
        }
        total_length += length;
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
bool reportAttribute() {
    size_t total_length = 0;
    size_t length = 0;
    size_t i = 0;
    wantSendData[0] = '{';
    total_length += 1;

    {=# attrs =}
    sprintf(tempSendData, FC(F("\"{= name =}\": %d,")), ({= type =}){= var =} / {= scale =});
    length = strlen(tempSendData);
    for (i=0; i<length; i++) {
        wantSendData[total_length + i] = tempSendData[i];
    }
    total_length += length;

    {=/ attrs =}
    {=# metrics =}
    requestValue[0] = '\0';
    dtostrf({= var =}_threshold, {= threshold_width =}, {= prec =}, requestValue);
    sprintf(tempSendData, FC(F("\"{= name =}_threshold\": %s,")), ltrim(requestValue));
    length = strlen(tempSendData);
    for (i=0; i<length; i++) {
        wantSendData[total_length + i] = tempSendData[i];
    }
    total_length += length;

    {=/ metrics =}
    wantSendData[total_length-1] = '}';
    wantSendData[total_length] = '\0';

    send_packet_1(ATTRIBUTE, wantSendData);
    return RET_SUCC;
}
{=/ use_eeprom =}

void reset() {
    wdt_disable();
    wdt_enable(WDTO_15MS);
    for (;;) {

    }
}
