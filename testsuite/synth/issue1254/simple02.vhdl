library ieee;
use ieee.std_logic_1164.all;

entity simple01 is
  port (a : in std_logic;
        z : out std_logic);
end simple01;

--use work.pkg.all;

architecture behav of simple01 is
begin
  process(A)
  begin
    Z <= not a;
  end process;
end behav;
