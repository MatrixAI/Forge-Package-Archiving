#!/bin/bash

start=4096
rm ./log
while [ $start -le 1048576 ]
do
	echo $start >> log
	sum=0
	./hashMultiPar $start +RTS -s 2>&1 | grep "Total" >> log
	./hashMultiPar $start +RTS -s 2>&1 | grep "Total" >> log
	./hashMultiPar $start +RTS -s 2>&1 | grep "Total" >> log
	./hashMultiPar $start +RTS -s 2>&1 | grep "Total" >> log
	./hashMultiPar $start +RTS -s 2>&1 | grep "Total" >> log
	./hashMultiPar $start +RTS -s 2>&1 | grep "Total" >> log
	./hashMultiPar $start +RTS -s 2>&1 | grep "Total" >> log
	./hashMultiPar $start +RTS -s 2>&1 | grep "Total" >> log
	./hashMultiPar $start +RTS -s 2>&1 | grep "Total" >> log
	./hashMultiPar $start +RTS -s 2>&1 | grep "Total" >> log
	tail -n 10 log | awk '{ sum += $3; n++ } END { if (n > 0) print "average="sum / n; }' >> log
	echo >> log
	(( start += $start ))
done
echo "Done"
