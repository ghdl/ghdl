-- Title      : Testcase for to_ux01 on a std_logic
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
entity test is

  port (
    sig_in : in std_logic_vector(1 downto 0);
    sig_out : out std_logic;
    sig_out_vec : out std_logic_vector(1 downto 0));
end entity test;

architecture str of test is

begin  -- architecture str
  sig_out <= to_ux01(sig_in(0)) and to_ux01(sig_in(1));
  sig_out_vec <= to_ux01(sig_in);
end architecture str;
