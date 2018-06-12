#!/bin/bash
# Usage: ./generate_n_nodes.sh N_SERVERS

n_servers=$1
cookie=secret
lan_ip="$(ifconfig eth0 | grep -Eo '(inet:)([0-9]*\.){3}[0-9]*' | cut -c 6-)"
pids=()

for i in $(seq 0 "$(($n_servers-1))"); do
	erl -name server$i@$lan_ip -setcookie $cookie -noshell &
	pids[$i]=$!
	done

echo "${pids[*]}"
for pid in "${pids[*]}"; do
	kill $pid
	done
