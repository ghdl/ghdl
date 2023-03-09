library ieee;
use ieee.std_logic_1164.all;

entity case07 is
  port (a : std_logic_vector (4 downto 0);
        o : out std_logic);
end case07;

architecture behav of case07 is
begin
  process (a)
  begin
    with a select o <=
      '1' when "00000",
      '0' when others;
  end process;
end behav;
