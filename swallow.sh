#!/usr/bin/env bash

# execute command and throw away output
# useful for save-commands
eval $@ &> /dev/null
