library ieee;
use ieee.std_logic_1164.all;

entity var06 is
  port (mask : std_logic_vector (1 downto 0);
        val : std_logic_vector (15 downto 0);
        res : out std_logic_vector (15 downto 0));
end var06;

architecture behav of var06 is
begin
  process (all)
    variable t : std_logic_vector (15 downto 0);
  begin
    t := (others => '0');
    if mask (0) = '1' then
      t (7 downto 0) := val (7 downto 0);
    end if;
    if mask (1) = '1' then
      t (15 downto 8) := val (15 downto 8);
    end if;
    res <= t;
  end process;
end behav;
