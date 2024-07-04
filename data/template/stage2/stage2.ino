{{={= =}=}}
#include "stage2.h"

{=# raws =}
{=& code =}

{=/ raws =}

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

{=# use_core1 =}
void setup1() {
    {=# setup1s =}
    {=& code =}

    {=/ setup1s =}
}

void loop1() {
    {=# loop1s =}
    {=& code =}
    {=/ loop1s =}
}

{=/ use_core1 =}
{=# functions =}
{= type =} {= name =}({=# argv =}{= type =} {= name =}{=^ last =}, {=/ last =}{=/ argv =}) {
    {=& code =}
}

{=/ functions =}

{=# func_raws =}
{=& code =}

{=/ func_raws =}
