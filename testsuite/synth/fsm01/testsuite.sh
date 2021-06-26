#! /bin/sh

. ../../testenv.sh

for t in fsm_2s fsm_3s fsm_4s fsm_5s fsm_6s fsm_7s; do
    synth_tb $t
done

echo "Test successful"
