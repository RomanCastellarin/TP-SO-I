#!/bin/bash
# Usage: ./generate_n_nodes.sh N_SERVERS SERVER_TO_CONNECT PORT COOKIE

N_SERVERS=$1
SERVER_TO_CONNECT=$2
PORT=$3
COOKIE=$4

LOCAL_IP="$(ifconfig eth0 | grep -Eo '(inet:)([0-9]*\.){3}[0-9]*' | cut -c 6-)"
PIDS=()

for i in $(seq 0 "$(($N_SERVERS-1))"); # for(i=0; i<N_SERVERS; i++)
do
#    echo "erl -name serverk$i@$LOCAL_IP -setcookie $COOKIE -noshell -eval \"server:startup($(($PORT+$i+1)),'$SERVER_TO_CONNECT').\""
	# Run the erlang node in the background
	erl -name serverk$i@$LOCAL_IP -setcookie $COOKIE -noshell -eval "server:startup($(($PORT+$i+1)),'$SERVER_TO_CONNECT')." &
	# Save node PID
	PIDS[$i]=$!
done

# Echo background PIDs
echo "${PIDS[*]}"

# Kill the generated nodes (for testing)
# for pid in "${PIDS[*]}"; do
# 	kill $pid
#	done
