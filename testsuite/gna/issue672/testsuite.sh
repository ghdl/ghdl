#! /bin/sh

. ../../testenv.sh

analyze SQR.vhd SQRTb.vhd
elab sqrtb
simulate sqrtb --stop-time=1us

if ! simulate --time-resolution=fs sqrtb --stop-time=1us; then
    echo "skip --time-resolution tests"
else
    simulate --time-resolution=ns sqrtb --stop-time=1us --disp-time | sed -e 's/.*info/info/' > sqrtb.out
    diff_nocr sqrtb.ref sqrtb.out
fi

rm -f sqrtb.out
clean

echo "Test successful"
