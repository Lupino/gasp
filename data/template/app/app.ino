{{={= =}=}}
{=# imports =}
#include {=& name =}
{=/ imports =}

{=# consts =}
{=# has_value =}
#define {= name =} {=& value =}
{=/ has_value =}
{=^ has_value =}
#define {= name =}
{=/ has_value =}
{=/ consts =}

{=# vars =}
{=# has_value =}
{= type =} {= name =} = {=& value =};
{=/ has_value =}
{=^ has_value =}
{= type =} {= name =};
{=/ has_value =}
{=/ vars =}

{=# has_app =}
#ifdef ARDUINO_ARCH_RP2040
#define EEPROM_NEED_COMMIT
#ifndef EEPROM_SIZE
#define EEPROM_SIZE 1024
#endif
#endif

{=/ has_app =}
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

void setup() {
    {=# setups =}
    {=& code =}

    {=/ setups =}
}

void loop() {
    {=# loops =}
    {=& code =}
    {=/ loops =}
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
}

{=/ functions =}
