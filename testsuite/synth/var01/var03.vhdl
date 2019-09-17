library ieee;
use ieee.std_logic_1164.all;

entity var03 is
  port (mask : std_logic_vector (1 downto 0);
        a, b : std_logic_vector (15 downto 0);
        res : out std_logic_vector (15 downto 0));
end var03;

architecture behav of var03 is
begin
  process (all)
    variable t : std_logic_vector (15 downto 0) := (others => '0');
    variable hi, lo : integer;
  begin
    t := a;
    for i in 0 to 1 loop
      if mask (i) = '1' then
        lo := i * 8;
        hi := lo + 7;
        t (hi downto lo) := b (hi downto lo);
      end if;
    end loop;
    res <= t;
  end process;
end behav;
