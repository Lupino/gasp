{{={= =}=}}
{=# imports =}
#include {=& name =}
{=/ imports =}
{=# consts =}
{=# has_value =}
{=^ has_type =}
#define {= name =} {= value =}
{=/ has_type =}
{=# has_type =}
{= type =} {= name =} = {= value =};
{=/ has_type =}
{=/ has_value =}
{=^ has_value =}
{=^ has_type =}
#define {= name =}
{=/ has_type =}
{=# has_type =}
{= type =} {= name =};
{=/ has_type =}
{=/ has_value =}
{=/ consts =}

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
// end defined
void setup() {
    #ifdef EEPROM_SIZE
    EEPROM.begin(EEPROM_SIZE);
    #endif
    {=# setups =}
    {=& code =}

    {=/ setups =}
}

void loop() {
    current_time_ms = get_current_time_ms();
    {=# loops =}
    {=& code =}
    {=/ loops =}
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
