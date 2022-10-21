#!/usr/bin/env bash

REMOTE_DIR=/home/dev/bench-1
ENV=bench

RUNS=$(ssh $ENV -- \
    sh -c "'cd $REMOTE_DIR/runs &&
        find . -mindepth 2 -maxdepth 2 -type f -name 'meta.json' -exec dirname \{\} \; |
        grep -v current\$\\|deploy-logs\$ |
        cut -c3- |
        sort ||
        true'" 2>/dev/null;)

RUN_ARR=( $RUNS )
echo "Cluster runs found: ${#RUN_ARR[@]}"

for run in "${RUN_ARR[@]}"
do
    dir=runs/$run
    
    if [ -d "$dir" ]; then
        echo "$dir exists: skipping"
    else
        echo "$dir fetching:"
        mkdir -p $dir/analysis
        scp -C $ENV:$REMOTE_DIR/runs/$run/meta.json $dir
        scp -C $ENV:$REMOTE_DIR/runs/$run/analysis/blockprop.json $dir/analysis
        scp -C $ENV:$REMOTE_DIR/runs/$run/analysis/clusterperf.json $dir/analysis
    fi
done
