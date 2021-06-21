library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity dpram2r is
  port (raddr : natural range 0 to 3;
        rnib  : natural range 0 to 1;
        rdat : out std_logic_vector (3 downto 0);
        waddr : natural range 0 to 3;
        wdat : std_logic_vector (7 downto 0);
        clk : std_logic);
end dpram2r;

architecture behav of dpram2r is
  type memtype is array (0 to 3) of std_logic_vector (7 downto 0);
  signal mem : memtype;
begin
  process (clk)
  begin
    if rising_edge (clk) then
      --  Not a memory: different widths
      rdat <= mem (raddr)(rnib * 4 + 3 downto rnib * 4);
      mem (waddr) <= wdat;
    end if;
  end process;
end behav;
