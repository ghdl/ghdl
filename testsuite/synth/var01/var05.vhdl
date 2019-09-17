library ieee;
use ieee.std_logic_1164.all;

entity var05 is
  port (sel : std_logic;
        a, b : std_logic_vector (1 downto 0);
        res : out std_logic_vector (1 downto 0));
end var05;

architecture behav of var05 is
begin
  process (all)
    variable idx : integer;
  begin
    res <= a;
    if sel = '1' then
      idx := 1;
      res <= b;
    end if;
  end process;
end behav;
