library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity dpram2 is
  port (raddr : std_logic_vector (3 downto 0);
        rdat : out std_logic_vector (7 downto 0);
        waddr : std_logic_vector (3 downto 0);
        wdat : std_logic_vector (7 downto 0);
        clk : std_logic);
end dpram2;

architecture behav of dpram2 is
begin
  process (clk)
    type memtype is array (15 downto 0) of std_logic_vector (7 downto 0);
    variable mem : memtype;
  begin
    if rising_edge (clk) then
      rdat <= mem (to_integer(unsigned (raddr)));
      mem (to_integer(unsigned (waddr))) := wdat;
    end if;
  end process;
end behav;
