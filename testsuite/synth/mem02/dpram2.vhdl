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
  subtype memtype is std_logic_vector (16 * 8 - 1 downto 0);
  signal mem : memtype;
begin
  process (clk)
    variable ra : natural;
    variable wa : natural;
    variable rlo, rhi : natural;
  begin
    if rising_edge (clk) then
      ra := to_integer(unsigned (raddr));
      rlo := ra * 8;
      rhi := rlo + 7;
      rdat <= mem (rhi downto rlo);
      wa := to_integer(unsigned (waddr));
      mem (wa * 8 + 7 downto wa * 8) <= wdat;
    end if;
  end process;
end behav;
