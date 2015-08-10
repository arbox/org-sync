#!/usr/bin/env bash

set -x

regexp_os='s/os-\(.*\)/org-sync-\1/g'

for i in org-sync*.el; do
    sed -e $regexp_os -i $i
done
