library ieee;
use ieee.std_logic_1164.all;

entity case01 is
  port (a : std_logic_vector(1 downto 0);
        o : out std_logic);
end;

architecture behav of case01 is
begin
  process(A)
  begin
    case a is
      when "00" | "01" | "10" =>
        o <= '1';
      when "11" =>
        o <= '0';
      when "XX" =>
        o <= '0';
      when others =>
        o <= '1';
    end case;
  end process;
end behav;
