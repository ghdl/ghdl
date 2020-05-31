library ieee;
use ieee.std_logic_1164.all;

entity simple01 is
  port (a, b, c : in std_logic;
        z : out std_logic);
end simple01;

architecture behav of simple01 is
begin
  process(A, B, C)
    variable temp : std_logic;
  begin
    case a is
      when '1' =>
        assert b = '0';
        z <= '0';
      when '0' =>
        z <= '1';
      when others =>
        z <= 'X';
    end case;
  end process;
end behav;
