library ieee;
use ieee.std_logic_1164.all;

entity case05 is
  port (a : integer range -3 to 2;
        o : out std_logic);
end;

architecture behav of case05 is
begin
  process (a)
  begin
    case a is
      when -2 downto -3 =>
        o <= '0';
      when -1 to 0 =>
        o <= '1';
      when 2 downto 1 =>
        o <= '0';
    end case;
  end process;
end behav;
