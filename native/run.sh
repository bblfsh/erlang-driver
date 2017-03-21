#!/bin/bash

rebar3 release
awk '{gsub("-noinput","");print}' _build/default/rel/pre-alpha/bin/pre-alpha > _build/default/rel/pre-alpha/bin/tmp
cat _build/default/rel/pre-alpha/bin/tmp > _build/default/rel/pre-alpha/bin/pre-alpha
rm _build/default/rel/pre-alpha/bin/tmp

_build/default/rel/pre-alpha/bin/pre-alpha foreground
