library ieee;
use ieee.std_logic_1164.all;

entity ret02 is
  port (di : std_logic_vector (7 downto 0);
        res : out integer);
end ret02;

architecture behav of ret02 is
  function sign (v : std_logic_vector (7 downto 0)) return integer is
  begin
    if v (7) = '1' then
      return -1;
    end if;
    return 1;
  end sign;
begin
  res <= sign (di);
end behav;
