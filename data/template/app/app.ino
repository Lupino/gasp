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

{=# raws =}
{=& code =}

{=/ raws =}
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
{= type =} {= name =}({=# argv =}{= type =} {= name =}{=^ last =}, {=/ last =}{=/ argv =});
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
{= type =} {= name =}({=# argv =}{= type =} {= name =}{=^ last =}, {=/ last =}{=/ argv =}) {
    {=& code =}
}

{=/ functions =}
