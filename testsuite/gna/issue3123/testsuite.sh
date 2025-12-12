#! /bin/sh

. ../../testenv.sh

analyze t18_Timer.vhdl
analyze t18_TimerTb.vhdl
elab t18_timertb

simulate t18_timertb --stop-time=1sec > timer.log
fgrep 'stopped by --stop-time @1000ms' timer.log

simulate t18_timertb --stop-time=1min > timer.log
fgrep 'stopped by --stop-time @60000ms' timer.log

simulate t18_timertb --stop-time=1hr > timer.log
fgrep 'stopped by --stop-time @3600000ms' timer.log

if simulate t18_timertb --stop-time=24hr > timer.log; then
    echo "error expected"
    exit 1;
fi
fgrep "time value '24hr' is too large" timer.log

clean

echo "Test successful"
