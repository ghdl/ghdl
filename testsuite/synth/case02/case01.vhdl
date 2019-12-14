library ieee;
use ieee.std_logic_1164.all;

entity case01 is
  port (a : std_logic_vector (1 downto 0);
        clk : std_logic;
        o : out std_logic_vector(1 downto 0));
end case01;

architecture behav of case01 is
begin
  process (clk)
  begin
    if rising_edge (clk) then
      case a is
        when "01" =>
          o (0) <= '1';
        when "11" =>
          o (1) <= '1';
        when "00" =>
          o (0) <= '0';
        when "10" =>
          o (1) <= '0';
        when others =>
          o <= "00";
      end case;
    end if;
  end process;
end behav;
