library ieee;
use ieee.std_logic_1164.all;

entity case01 is
  port (a : std_logic_vector (4 downto 0);
        o : out std_logic);
end case01;

architecture behav of case01 is
begin
  process (a)
  begin
    o <= '0';
    case a is
      when "00011" =>
        o <= '1';
      when "00110" | "00111" | "10001" =>
        o <= '1';
      when "00100" =>
      when "01100" =>
        o <= '1';
      when "10000" =>
        o <= '1';
      when others =>
        o <= '0';
    end case;
  end process;
end behav;
