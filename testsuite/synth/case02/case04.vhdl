library ieee;
use ieee.std_logic_1164.all;

entity case04 is
  port (a : natural range 0 to 3;
        o : out std_logic);
end;

architecture behav of case04 is
begin
  process (a)
  begin
    case a is
      when 1 =>
        o <= '1';
      when others =>
        o <= '0';
    end case;
  end process;
end behav;
