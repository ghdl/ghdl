library ieee;
use ieee.std_logic_1164.all;

entity case02 is
  port (a : std_logic_vector (1 downto 0);
        o : out std_logic);
end case02;

architecture behav of case02 is
begin
  process (a)
  begin
    case a(1 downto 0) is
      when "01" =>
        o <= '1';
      when others =>
        o <= '0';
    end case;
  end process;
end behav;
