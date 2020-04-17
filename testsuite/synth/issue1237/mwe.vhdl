library ieee;
use ieee.std_logic_1164.all;

entity MWE is
end MWE;

architecture test of MWE is
  constant P       : integer := 1;
  signal   my_sig  : std_logic_vector(P downto 0);
begin
  block2: if P = 2 generate
    my_sig(2) <= '1';
  end generate;

  block1: if P = 1 generate
    my_sig(1) <= '1';
  end generate;

  -- even this block alone breaks during analysis
  blockf: if false generate
    my_sig(2) <= '1';
  end generate;
end architecture;
