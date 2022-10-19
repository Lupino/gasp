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
{= type =} {=& name =} = {=& value =};
{=/ has_value =}
{=^ has_value =}
{= type =} {=& name =};
{=/ has_value =}
{=/ vars =}

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
