library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity dpram1r is
  port (raddr : natural range 0 to 3;
        rbit  : natural range 0 to 7;
        rdat : out std_logic;
        waddr : natural range 0 to 3;
        wdat : std_logic_vector (7 downto 0);
        clk : std_logic);
end dpram1r;

architecture behav of dpram1r is
  type memtype is array (0 to 3) of std_logic_vector (7 downto 0);
  signal mem : memtype;
begin
  process (clk)
  begin
    if rising_edge (clk) then
      --  Not a memory: uses different widths.
      rdat <= mem (raddr)(rbit);
      mem (waddr) <= wdat;
    end if;
  end process;
end behav;
