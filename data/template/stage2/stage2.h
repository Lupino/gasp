{{={= =}=}}

#ifndef STAGE2_H
#define STAGE2_H

{=# imports =}
#include {=& name =}
{=/ imports =}

#ifdef __cplusplus
extern "C" {
#endif

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


#ifdef __cplusplus
} /*extern "C"*/
#endif

#endif
