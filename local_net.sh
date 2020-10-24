#!/bin/bash

python3 make_daemon.py

CODA_AUTOMATION_SCRIPTS=../../coda-automation/master/scripts
BIN=_build/default/src/app/cli/src/coda.exe

screen -dmS coda -t win0
screen -S coda -X screen -t win1
screen -S coda -X screen -t win2
#screen -S coda -X screen -t win3
#screen -S coda -X screen -t win4

rm -rf /tmp/codaA
rm -rf /tmp/codaB
rm -rf /tmp/codaC

mkdir -p /tmp/codaA/wallet
mkdir -p /tmp/codaB/wallet
mkdir -p /tmp/codaC/wallet

mkdir -p /tmp/codaB/wallet2

mkdir -p /tmp/codaA/config
mkdir -p /tmp/codaB/config
mkdir -p /tmp/codaC/config

cp daemon.json /tmp/codaA/

cp daemon.json /tmp/codaB/
cp $CODA_AUTOMATION_SCRIPTS/online_whale_keys/online_whale_account_1 /tmp/codaB/wallet/key
cp $CODA_AUTOMATION_SCRIPTS/online_whale_keys/online_whale_account_1.pub /tmp/codaB/wallet/key.pub
cp $CODA_AUTOMATION_SCRIPTS/offline_whale_keys/offline_whale_account_1 /tmp/codaB/wallet2/key
cp $CODA_AUTOMATION_SCRIPTS/offline_whale_keys/offline_whale_account_1.pub /tmp/codaB/wallet2/key.pub

cp daemon.json /tmp/codaC/
cp $CODA_AUTOMATION_SCRIPTS/online_fish_keys/online_fish_account_1 /tmp/codaC/wallet/key
cp $CODA_AUTOMATION_SCRIPTS/online_fish_keys/online_fish_account_1.pub /tmp/codaC/wallet/key.pub


chmod -R 0700 /tmp/codaA
chmod -R 0700 /tmp/codaB
chmod -R 0700 /tmp/codaC


screen -S coda -p win0 -X stuff "./$BIN"$' daemon -seed -client-port 3000 -rest-port 3001 -external-port 3002 -config-directory /tmp/codaA/config -config-file /tmp/codaA/daemon.json -generate-genesis-proof true -discovery-keypair CAESQNf7ldToowe604aFXdZ76GqW/XVlDmnXmBT+otorvIekBmBaDWu/6ZwYkZzqfr+3IrEh6FLbHQ3VSmubV9I9Kpc=,CAESIAZgWg1rv+mcGJGc6n6/tyKxIehS2x0N1Uprm1fSPSqX,12D3KooWAFFq2yEQFFzhU5dt64AWqawRuomG9hL8rSmm5vxhAsgr -log-json -log-level Spam | tee /tmp/codaA/log | _build/default/src/app/logproc/logproc.exe -f \'.level in ["Trace", "Debug", "Info", "Warn", "Error", "Faulty_peer", "Fatal"]\' \n'

sleep 3

screen -S coda -p win1 -X stuff "CODA_PRIVKEY_PASS=\"naughty blue worm\" ./$BIN"$' daemon -peer "/ip4/127.0.0.1/tcp/3002/p2p/12D3KooWAFFq2yEQFFzhU5dt64AWqawRuomG9hL8rSmm5vxhAsgr" -client-port 4000 -rest-port 4001 -external-port 4002 -config-directory /tmp/codaB/config -config-file /tmp/codaB/daemon.json -generate-genesis-proof true -block-producer-key /tmp/codaB/wallet/key -log-json -run-snark-worker B62qp4UturELw4MmhAZhor8rwzaH1BBAivRnvdp1Yhkq6odhhFiT8uC -work-selection seq -log-level Spam | tee /tmp/codaB/log | _build/default/src/app/logproc/logproc.exe -f \'.level in ["Trace", "Debug", "Info", "Warn", "Error", "Faulty_peer", "Fatal"]\'\n'

#screen -S coda -p win2 -X stuff "sleep 30; ./$BIN"$' internal snark-worker -proof-level check -daemon-address "127.0.0.1:4000"\n'

screen -S coda -p win2 -X stuff "sleep 90; CODA_PRIVKEY_PASS=\"naughty blue worm\" ./$BIN"$' daemon -peer "/ip4/127.0.0.1/tcp/3002/p2p/12D3KooWAFFq2yEQFFzhU5dt64AWqawRuomG9hL8rSmm5vxhAsgr" -client-port 5000 -rest-port 5001 -external-port 5002 -config-directory /tmp/codaC/config -config-file /tmp/codaC/daemon.json -generate-genesis-proof true -block-producer-key /tmp/codaC/wallet/key -log-json -log-level Spam | tee /tmp/codaC/log | _build/default/src/app/logproc/logproc.exe -f \'.level in ["Trace", "Debug", "Info", "Warn", "Error", "Faulty_peer", "Fatal"]\'\n'

for i in {3..50} ; do
  OUTDIR="/tmp/coda_$i"
  rm -rf $OUTDIR
  mkdir -p $OUTDIR/wallet
  mkdir -p $OUTDIR/config
  cp daemon.json "$OUTDIR/"

  CLIENT=$(expr 5100 + $i)
  REST=$(expr 6100 + $i)
  EXTERNAL=$(expr 7100 + $i)

  screen -S coda -X screen -t win$i
  screen -S coda -p win$i -X stuff "sleep 90; ./$BIN"$' daemon -peer "/ip4/127.0.0.1/tcp/3002/p2p/12D3KooWAFFq2yEQFFzhU5dt64AWqawRuomG9hL8rSmm5vxhAsgr" -client-port'" $CLIENT -rest-port $REST -external-port $EXTERNAL -config-directory $OUTDIR/config -config-file $OUTDIR/daemon.json "$'-generate-genesis-proof true -log-json -log-level Spam | '"tee $OUTDIR/log"$' | _build/default/src/app/logproc/logproc.exe -f \'.level in ["Trace", "Debug", "Info", "Warn", "Error", "Faulty_peer", "Fatal"]\'\n'
done

screen -r
