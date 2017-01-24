library ieee;
use ieee.std_logic_1164.all;

entity repro is
end;

architecture behav of repro is
begin
  process
    variable result : std_ulogic;
  begin
    result := std_ulogic_vector'("-000") ?/= std_ulogic_vector'("0000");
    result := std_ulogic_vector'("-000") ?= std_ulogic_vector'("0000");
    wait;
  end process;
end behav;
