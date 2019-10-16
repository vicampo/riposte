#!/bin/bash

secret_password='SECRET!'
opt_timestamp=''
opt_password=''
opt_quiet=''
now=$(date -u)
who=$(whoami)

while [[ $# -gt 0 ]]
do
    key="$1"
    case $key in
	-q|--quiet)
	    opt_quiet='y'
	    shift
	    ;;
	-t|--timestamp)
	    opt_timestamp='y'
	    shift
	    ;;
	-p|--password)
	    opt_password='y'
	    shift
	    ;;
	*)
	    echo "Unknown option '${key}'."
	    exit 1
	    ;;
    esac
done

# running quietly means not doing anything
if [ "${opt_quiet}" = "y" ]; then
    exit 0;
fi

if [ "${opt_password}" = "y" ]; then
    if [ "${opt_timestamp}" = "y" ]; then
	echo $(printf '{"username":"%s","password":"%s","timestamp":"%s"}' "${who}" "${secret_password}" "${now}")
    else
	echo $(printf '{"username":"%s","password":"%s"}' "${who}" "${secret_password}")
    fi
elif [ "${opt_timestamp}" = "y" ]; then
    echo $(printf '{"username":"%s","timestamp":"%s"}' "${who}" "${now}")
else
    echo $(printf '{"username":"%s"}' "${who}")
fi
