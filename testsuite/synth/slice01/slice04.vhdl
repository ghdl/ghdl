library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity slice04 is
  port (clk : std_logic;
        dat : std_logic_vector (7 downto 0);
        mask : std_logic_vector (1 downto 0);
        res : out std_logic_vector (7 downto 0));
end slice04;

architecture behav of slice04 is
  signal z : natural range 0 to 0;
  signal mem : std_logic_vector (7 downto 0);
begin
  z <= to_integer(unsigned(mask));

  process(clk)
    variable hi, lo : natural;
  begin
    if rising_edge (clk) then
      mem (z*3 + 7 downto z*3) <= dat;
    end if;
  end process;

  res <= mem;
end behav;
