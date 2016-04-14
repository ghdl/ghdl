#! /bin/sh

. ../../testenv.sh

files="
pkg_tb.vhd
add_118.vhd
add_134.vhd
add_170.vhd
add_171.vhd
add_188.vhd
add_214.vhd
add_220.vhd
assert_uut.vhd
cmp_111.vhd
cmp_112.vhd
cmp_113.vhd
cmp_114.vhd
cmp_115.vhd
cmp_119.vhd
cmp_126.vhd
cmp_127.vhd
cmp_128.vhd
cmp_129.vhd
cmp_130.vhd
cmp_131.vhd
cmp_132.vhd
cmp_133.vhd
cmp_135.vhd
cmp_136.vhd
cmp_137.vhd
cmp_138.vhd
cmp_139.vhd
cmp_140.vhd
cmp_141.vhd
cmp_142.vhd
cmp_146.vhd
cmp_148.vhd
cmp_150.vhd
cmp_152.vhd
cmp_154.vhd
cmp_156.vhd
cmp_158.vhd
cmp_160.vhd
cmp_174.vhd
cmp_176.vhd
cmp_178.vhd
cmp_180.vhd
cmp_183.vhd
cmp_185.vhd
cmp_187.vhd
cmp_191.vhd
cmp_193.vhd
cmp_195.vhd
cmp_198.vhd
cmp_200.vhd
cmp_202.vhd
cmp_204.vhd
cmp_215.vhd
cmp_216.vhd
cmp_217.vhd
cmp_218.vhd
cmp_221.vhd
fsm.vhd
fsm_15.vhd
muxb_117.vhd
muxb_120.vhd
muxb_121.vhd
muxb_123.vhd
muxb_124.vhd
muxb_219.vhd
prog.vhd
sub_125.vhd
sub_145.vhd
sub_189.vhd
sync_ram.vhd
v_split0.vhd
v_split1.vhd
v_split2.vhd
v_split3.vhd
v_split4.vhd
v_split5.vhd
v_split6.vhd
v_split7.vhd
w_split0.vhd
w_split1.vhd
w_split2.vhd
w_split3.vhd
w_split4.vhd
w_split5.vhd
w_split6.vhd
w_split7.vhd
top.vhd
cp3_test.vhd
tb.vhd
"

for f in $files; do
  analyze vector.d/$f
done

elab_simulate tb

clean

echo "Test successful"
