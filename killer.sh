#!/bin/zsh

./"$1" &
while (( $(grep MemFree /proc/meminfo | awk '{print $2}') > 1500000 ))
do
	sleep 0.1
done
echo RACKET IS USING TOO MUCH MEMORY, ABORTING!
kill -9 $(ps aux | grep "[r]acket" | awk '{print $2}')
