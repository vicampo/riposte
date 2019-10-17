#!/bin/bash

set -euo pipefail

if [ -z "${1}" ]; then
    echo "Usage: check.sh RIPOSTE-FILE"
    exit 1
fi

$fileToCheck = $1

if [ ! -e "${fileToCheck}" ]; then
    echo "Error: No such file '${fileToCheck}'."
    exit 1
fi

$basenameOfFileToCheck = basename "${fileToCheck}"
$serverToUse = "${basenameOfFileToCheck}.rkt"

if [ ! -e "${serverToUse} "]; then

fi

$ripostePath = ../riposte.rkt

if [ ! -e "${ripostePath}" ]; then
    echo "Error: riposte.rkt not found at ${ripostePath}."
    exit 1
fi

racket "${ripostePath}"
