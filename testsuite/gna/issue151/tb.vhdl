library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test is
 port (in_vec   : in std_logic_vector);
end entity;

architecture rtl of test is
signal A : natural;
begin

  gen2 : if in_vec'length <= 2 generate
     A <= 2;
  end generate;
  gen3 : if in_vec'length > 2 generate
     A <= 3;
  end generate;
end architecture;
