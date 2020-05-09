library ieee;
use ieee.std_logic_1164.all;

entity case04 is
  port (a : std_logic_vector (4 downto 0);
        o : out std_logic);
end case04;

architecture behav of case04 is
begin
  process (a)
  begin
    o <= '0';
    case a is
      when others =>
        o <= '1';
    end case;
  end process;
end behav;
