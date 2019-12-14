library ieee;
use ieee.std_logic_1164.all;

entity conv01 is
  port (i : std_logic_vector (19 downto 0);
        o : out std_logic_vector (31 downto 0));
end conv01;

architecture behav of conv01 is
begin
  process (i)
    variable v : std_logic_vector (31 downto 0);
  begin
    v := (i'left downto 0 => i, others => '0');
    o <= v;
  end process;
end behav;
