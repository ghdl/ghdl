library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
-- use std.env.finish;

entity tb_slice_bug is
end entity;

architecture test of tb_slice_bug is

  signal divindex : std_logic_vector(31 downto 0);
  signal indata : std_logic_vector(63 downto 0);
  signal outdata : std_logic_vector(31 downto 0);

begin

slice_bug_p: entity work.slice_bug(rtl)
port map (
  clock_i => '0', 
  divindex => divindex,
  indata => indata,
  outdata => outdata);

stimulus: process
begin

  divindex <= std_logic_vector(to_unsigned(12, 32));
  indata <= x"0BEBE0000CACA000";
  wait for 1 ns;
  assert outdata = x"BEBECACA" report "Wrong output data" severity error;
  wait;

end process stimulus;
end architecture test;
