#! /bin/sh

. ../../testenv.sh

units="
call1
call2
call3
call4
call5
call6
call6a
call6b
call7
call8
call9
call10
for1
if1
if2
if3
if4
loop1
loop2
case1
case3
case4
ret1
wait1
wait2
wait3
"

for f in $units; do
  analyze ${f}.vhdl
  elab_simulate $f --assert-level=error 2>&1 | tee res.out
  if ! grep -q SUCCESS res.out; then
     echo "Failure of $f"
     exit 1
  fi
done

rm -f res.out
clean

echo "Test successful"
