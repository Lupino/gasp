{{={= =}=}}
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

#ifdef SEND_DELAY_MS
unsigned long send_timer_ms = get_current_time_ms();
#endif

#ifndef MAX_GL_PAYLOAD_LENGTH
#define MAX_GL_PAYLOAD_LENGTH 127
#endif
#ifndef WANT_SEND_DATA_LENGTH
#define WANT_SEND_DATA_LENGTH 50
#endif

#ifndef MAX_NUM_TOKENS
#define MAX_NUM_TOKENS 10
#endif

#ifndef MAX_VALUE_LENGTH
#define MAX_VALUE_LENGTH 10
#endif

uint16_t lastPayloadId = 0;
uint16_t readedLen = 0;
uint8_t  readedPayload[MAX_GL_PAYLOAD_LENGTH + 1];
uint8_t  sendedPayload[MAX_GL_PAYLOAD_LENGTH + 1];

bool authedLock = false;

givelink_t * m = givelink_new(MAX_GL_PAYLOAD_LENGTH);

char wantSendData[WANT_SEND_DATA_LENGTH];
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
    // put your setup code here, to run once:
    {=# app =}
    givelink_init("{= key =}", "{= token =}");
    {=/ app =}

    {=# use_eeprom =}
    byte first_run_flag = 0;
    EEPROM.get(0, first_run_flag);
    {=/ use_eeprom =}

    {=# attrs =}
    if (first_run_flag == 1) {
        EEPROM.get({= addr =}, {= var =});
        if ({= var =} > {= max =} || {= var =} < {= min =}) {
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
    {=# use_eeprom =}
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
        if (!authedLock) {
            authedLock = false;
            reportAttribute();
        }
        {=# has_metric =}
        #ifdef SEND_DELAY_MS
        if (send_timer_ms + SEND_DELAY_MS < get_current_time_ms() || checkValue()) {
        #else
        if (checkValue()) {
        #endif
            processTelemetries();
        }
        {=/ has_metric =}
    } else {
        authedLock = true;
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
    #ifdef SEND_DELAY_MS
    send_timer_ms = get_current_time_ms();
    #endif
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

bool jsoneq(const char *json, jsmntok_t *tokens, const char *s) {
  if (tokens->type == JSMN_STRING && (int)strlen(s) == tokens->end - tokens->start &&
      strncmp(json + tokens->start, s, tokens->end - tokens->start) == 0) {
    return true;
  }
  return false;
}

bool jsonlookup(const char *json, jsmntok_t *tokens, int num_tokens, const char *name, char *value) {
    /* Assume the top-level element is an object */
    if (num_tokens < 1 || tokens[0].type != JSMN_OBJECT) {
      return false;
    }
    /* Loop over all keys of the root object */
    for (int i = 1; i < num_tokens; i++) {
        if (jsoneq(json, &tokens[i], name)) {
            /* We may use strndup() to fetch string value */
            sprintf(value, "%.*s", tokens[i + 1].end - tokens[i + 1].start,
                   json + tokens[i + 1].start);
            return true;
        }
    }
    return false;
}

{=# attrs =}
int set_{= var =}(const char *json, jsmntok_t *tokens, int num_tokens, char *retval) {
    char value[MAX_VALUE_LENGTH];
    if (jsonlookup(json, tokens, num_tokens, "data", value)) {
        {= type =} tmp = atoi(value);
        if (tmp > {= max =} || tmp < {= min =}) {
          sprintf(retval, FC(F("{\"err\": \"data must between: [{= min =}, {= max =}]\"}")));
          return RET_ERR;
        }
        {= var =} = tmp * {= scale =};
        EEPROM.put({= addr =}, tmp);
    }
    sprintf(retval, FC(F("{\"{= name =}\": %d}")), {= var =});
    return RET_ATTR;
}
int get_{= var =}(char *retval) {
    sprintf(retval, FC(F("{\"{= name =}\": %d}")), ({= type =}){= var =} / {= scale =});
    return RET_SUCC;
}

{=/ attrs =}
{=# metrics =}
int set_{= var =}_threshold(const char *json, jsmntok_t *tokens, int num_tokens, char *retval) {
    char value[MAX_VALUE_LENGTH];
    if (jsonlookup(json, tokens, num_tokens, "data", value)) {
        {= type =} tmp = atof(value);
        if (tmp < {= min_threshold =} || tmp > {= max_threshold =}) {
          sprintf(retval, FC(F("{\"err\": \"data must between: [{= min_threshold =}, {= max_threshold =}]\"}")));
          return RET_ERR;
        }
        {= var =}_threshold = tmp;
        EEPROM.put({= addr =}, {= var =}_threshold);
    }
    value[0] = '\0';
    dtostrf({= var =}_threshold, {= width =}, {= prec =}, value);
    sprintf(retval, FC(F("{\"{= name =}_threshold\": %s}")), value);
    return RET_ATTR;
}

int get_{= var =}_threshold(char *retval) {
    char tmp[MAX_VALUE_LENGTH];
    dtostrf({= var =}_threshold, {= width =}, {= prec =}, tmp);
    sprintf(retval, FC(F("{\"{= name =}_threshold\": %s}")), tmp);
    return RET_SUCC;
}

int invalid_error(char *retval) {
    sprintf(retval, FC(F("{\"err\": \"{= name =} is invalid\"}")));
    return RET_ERR;
}

int get_{= var =}(char *retval) {
    if (isnan({= var =})) {
        return invalid_error(retval);
    }
    if ({= var =} < {= min =}) {
        return invalid_error(retval);
    }
    if ({= var =} > {= max =}) {
        return invalid_error(retval);
    }
    char tmp[MAX_VALUE_LENGTH];
    dtostrf({= var =}, {= width =}, {= prec =}, tmp);
    last_{= var =} = {= var =};
    sprintf(retval, FC(F("{\"{= name =}\": %s}")), tmp);
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
    jsmn_parser parser;
    jsmntok_t tokens[MAX_NUM_TOKENS]; /* We expect no more than 128 tokens */
    char method[{= max_cmd_len =}];

    /* Prepare parser */
    jsmn_init(&parser);
    int num_tokens = jsmn_parse(&parser, json, length, tokens, MAX_NUM_TOKENS);

    if (num_tokens < 0) {
        sprintf(retval, FC(F("{\"err\": \"Failed to parse JSON: %d\"}")), num_tokens);
        return RET_ERR;
    }

    if (jsonlookup(json, tokens, num_tokens, "method", method)) {
        {=# commands =}
        if (strcmp("{= name =}", method) == 0) {
            {=# flag =}
            {=# retval =}
            {=# json =}
            int r = {= fn =}(json, tokens, num_tokens, retval);
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
        if (strcmp("set_{= name =}", method) == 0) {
            int r = set_{= var =}(json, tokens, num_tokens, retval);
            if (r > RET_ERR) {
                return r;
            } else {
                sprintf(retval, FC(F("{\"err\": \"set_{= name =} error\"}")));
                return RET_ERR;
            }
        }
        if (strcmp("get_{= name =}", method) == 0) {
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
        if (strcmp("set_{= name =}_threshold", method) == 0) {
            int r = set_{= var =}_threshold(json, tokens, num_tokens, retval);
            if (r > RET_ERR) {
                return r;
            } else {
                sprintf(retval, FC(F("{\"err\": \"set_{= name =}_threshold error\"}")));
                return RET_ERR;
            }
        }
        if (strcmp("get_{= name =}_threshold", method) == 0) {
            int r = get_{= var =}_threshold(retval);
            if (r > RET_ERR) {
                return r;
            } else {
                sprintf(retval, FC(F("{\"err\": \"get_{= name =}_threshold error\"}")));
                return RET_ERR;
            }
        }
        if (strcmp("get_{= name =}", method) == 0) {
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
    {=# metrics =}
    wantSendData[0] = '\0';
    if (get_{= name =}(wantSendData) > RET_ERR) {
      send_packet_1(TELEMETRY, wantSendData);
      ret = true;
    }

    {=/ metrics =}
    return ret;
}

{=# has_metric =}
bool checkValue() {
    {=# metrics =}
    if (!isnan({= var =}) && {= var =} >= {= min =} && {= var =} <= {= max =} && abs(last_{= var =} - {= var =}) > {= var =}_threshold) {
        return true;
    }

    {=/ metrics =}
    return false;
}
{=/ has_metric =}

bool reportAttribute() {
    bool ret = false;
    {=# attrs =}
    wantSendData[0] = '\0';
    if (get_{= var =}(wantSendData) > RET_ERR) {
      send_packet_1(ATTRIBUTE, wantSendData);
      ret = true;
    }

    {=/ attrs =}
    {=# metrics =}
    wantSendData[0] = '\0';
    if (get_{= var =}_threshold(wantSendData) > RET_ERR) {
      send_packet_1(ATTRIBUTE, wantSendData);
      ret = true;
    }

    {=/ metrics =}
    return ret;
}
