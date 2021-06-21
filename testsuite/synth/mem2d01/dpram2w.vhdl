library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity dpram2w is
  port (waddr : natural range 0 to 3;
        wnib  : natural range 0 to 1;
        wdat : std_logic_vector (3 downto 0);
        raddr : natural range 0 to 3;
        rdat : out std_logic_vector (7 downto 0);
        clk : std_logic);
end dpram2w;

architecture behav of dpram2w is
  type memtype is array (0 to 3) of std_logic_vector (7 downto 0);
  signal mem : memtype;
begin
  process (clk)
  begin
    if rising_edge (clk) then
      --  Not a memory: different widths
      mem (waddr)(wnib * 4 + 3 downto wnib * 4) <= wdat;
      rdat <= mem (raddr);
    end if;
  end process;
end behav;
