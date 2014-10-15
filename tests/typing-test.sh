#!/bin/bash

ALL_OFF="\x1b[1;0m"
BOLD="\x1b[1;1m"
BLUE="\x1b[1;34m"
GREEN="\x1b[1;32m"
RED="\x1b[1;31m"
YELLOW="\x1b[1;33m"

printf "${BOLD}Starting typing tests..${ALL_OFF}\n"

tests="0"
errors="0"

for f in typing-*.c
do
    ../c2bf.native "$f" 2>/dev/null

    if [[ $? = 0 ]]; then
        printf "  $f ... ${BOLD}${RED}ERROR${ALL_OFF}\n"
        let "errors = $errors + 1"
    else
        printf "  $f ... ${BOLD}OK${ALL_OFF}\n"
    fi

    let "tests = $tests + 1"
done

printf "\n${BOLD}Results :${ALL_OFF}\n"
if [[ $errors > 0 ]]; then
    printf "  ${BOLD}${RED}$errors test(s) failed.${ALL_OFF}\n"
else
    printf "  ${BOLD}${GREEN}$tests tests passed successfully.${ALL_OFF}\n"
fi
