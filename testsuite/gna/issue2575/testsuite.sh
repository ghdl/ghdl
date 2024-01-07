#! /bin/sh

. ../../testenv.sh

analyze test.vhdl
STATS=$(elab_simulate test --stats)
echo "$STATS" | head -n 1
clean

NBR_OF_EVENTS=$(echo "$STATS" | sed -rn "s/^Nbr of events: ([0-9]+)/\1/p")
NBR_OF_ACTIVE=$(echo "$STATS" | sed -rn "s/^Nbr of active: ([0-9]+)/\1/p")

ERROR=0

if [ "$NBR_OF_EVENTS" != "3" ]; then
  echo "ERROR: Nbr of events: Expected: 3, Actual: $NBR_OF_EVENTS"
  ERROR=1
fi

if [ "$NBR_OF_ACTIVE" != "3" ]; then
  echo "ERROR: Nbr of active: Expected: 3, Actual: $NBR_OF_ACTIVE"
  ERROR=1
fi

if [ "$ERROR" -eq "0" ]; then
  echo "Test successful"
else
  echo "Test failed"
fi
