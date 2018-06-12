#!/bin/bash
# Usage: ./generate_n_nodes.sh N_SERVERS COOKIE

N_SERVERS=$1
COOKIE=$2
HOST_IP="$(ifconfig eth0 | grep -Eo '(inet:)([0-9]*\.){3}[0-9]*' | cut -c 6-)"
PIDS=()

for i in $(seq 0 "$(($N_SERVERS-1))"); # for(i=0; i<N_SERVERS; i++)
do
	if [ $i -eq 0 ]	
	then # It's the first server
		erl -name server$i@$HOST_IP -setCOOKIE $COOKIE -noshell &
	else # Ping myself from the previous generated server 
		erl -name server$i@$HOST_IP -setCOOKIE $COOKIE -noshell -eval 'rpc:call(server$(($i-1))@$HOST_IP, net_adm, ping, [server$i@$HOST_IP]).' &
	fi
	PIDS[$i]=$! # Save node PID
done

echo "${PIDS[*]}" # Echo background PIDs
for pid in "${PIDS[*]}"; do # Kill the nodes (for testing)
	kill $pid
	done
