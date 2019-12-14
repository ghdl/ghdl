library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity dpram1 is
  port (raddr : std_logic_vector (3 downto 0);
        rdat : out std_logic_vector (7 downto 0);
        waddr : std_logic_vector (3 downto 0);
        wdat : std_logic_vector (7 downto 0);
        clk : std_logic);
end dpram1;

architecture behav of dpram1 is
  type memtype is array (15 downto 0) of std_logic_vector (7 downto 0);
  signal mem : memtype;
begin
  process (clk)
  begin
    if rising_edge (clk) then
      rdat <= mem (to_integer(unsigned (raddr)));
      mem (to_integer(unsigned (waddr))) <= wdat;
    end if;
  end process;
end behav;
