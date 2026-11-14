#! /bin/sh

. ../../testenv.sh

#  The analysis time stamps come from the clock, which can go backward (when
#  it is adjusted).  Simulate that by editing the time stamps in the library.

#  Set the analysis time stamp of FILE in library file CF to STAMP.
set_stamp ()
{
  sed -e "s/^\(file . \"$2\" \"[0-9a-f]*\"\) \"[0-9.]*\"/\1 \"$3\"/" < $1 > $1.tmp
  mv $1.tmp $1
  grep -q "\"$2\" \"[0-9a-f]*\" \"$3\"" $1
}

#  Same library: b is analysed after a, but has an earlier time stamp.  It
#  is not obsolete.
analyze a.vhdl
analyze b.vhdl
set_stamp work-obj93.cf a.vhdl 20260101000001.000
set_stamp work-obj93.cf b.vhdl 20260101000000.000
elab_simulate b

#  But re-analysing a still makes b obsolete.
analyze a.vhdl
elab_failure b

clean

#  Other library: lib1.a looks analysed in the future (the clock went
#  backward since).  c, analysed after it, must not look older.
analyze --work=lib1 a.vhdl
set_stamp lib1-obj93.cf a.vhdl 29991231235959.999
analyze c.vhdl
elab_simulate c

clean
clean lib1

echo "Test successful"
