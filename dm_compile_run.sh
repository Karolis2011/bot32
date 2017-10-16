#! /bin/bash

#Get timeout from https://github.com/pshved/timeout
timeout="$HOME/timeout --just-kill --no-info-on-success --detect-hangups -h 10 -t 10 -m 102400"

file=$1

if [[ $2 == "run" ]]; then
	DreamDaemon $file.dmb -invisible -ultrasafe 2>&1 | tail -n +4 | head -c 512
	exit 0
fi

cd dm/

export BYOND_SYSTEM=$HOME/byond
export PATH=$HOME/byond/bin:$PATH
export LD_LIBRARY_PATH=$HOME/byond/bin:$LD_LIBRARY_PATH

output=$($timeout DreamMaker $file.dme 2>&1)
return=$?

if [[ $return != 0 ]]; then
	echo "$output" | tail -n +3
else
	if [[ -e $file.rsc ]]; then
		echo "You attempted to use a resource file; this is blocked for security reasons."
	else
		$timeout ../$0 $file run
	fi
fi

rm $file.*
