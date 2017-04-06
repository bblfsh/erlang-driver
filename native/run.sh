#!/bin/bash

#This script is used only for development purposes
#Run.sh builds the native part of the driver without anything related with bblfs-sdk
rebar3 release
awk '{gsub("-noinput","");print}' _build/default/rel/pre-alpha/bin/pre-alpha > _build/default/rel/pre-alpha/bin/tmp
cat _build/default/rel/pre-alpha/bin/tmp > _build/default/rel/pre-alpha/bin/pre-alpha
rm _build/default/rel/pre-alpha/bin/tmp

exec _build/default/rel/pre-alpha/bin/pre-alpha foreground
