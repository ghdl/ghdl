library ieee;
use ieee.std_logic_1164.all;

entity var04 is
  port (mask : std_logic_vector (1 downto 0);
        val : std_logic_vector (15 downto 0);
        res : out std_logic_vector (15 downto 0));
end var04;

architecture behav of var04 is
begin
  process (all)
    variable t : std_logic_vector (15 downto 0);
    variable hi, lo : integer;
  begin
    t := (others => '0');
    for i in 0 to 1 loop
      if mask (i) = '1' then
        lo := i * 8;
        hi := lo + 7;
        t (hi downto lo) := val (hi downto lo);
      end if;
    end loop;
    res <= t;
  end process;
end behav;
