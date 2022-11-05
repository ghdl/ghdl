library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity dpram6 is
  port (raddr : std_logic_vector (3 downto 0);
        rdat : out std_logic_vector (7 downto 0);
        waddr : std_logic_vector (3 downto 0);
        wdat : std_logic_vector (7 downto 0);
        clk : std_logic);
end;

architecture behav of dpram6 is
  subtype memtype is std_logic_vector (16 * 8 - 1 downto 0);
begin
  process (clk)
    variable ra : natural;
    variable wa : natural;
    variable mem : memtype;
  begin
    --  Not correct (?)
    --  Synthesized netlist is parallel while this code is sequential.
    if rising_edge(clk) then
      wa := to_integer(unsigned (waddr));
      mem (wa * 8 + 7 downto wa * 8) := wdat;
    end if;
    if rising_edge (clk) then
      ra := to_integer(unsigned (raddr));
      rdat <= mem (ra * 8 + 7 downto ra * 8);
    end if;
  end process;
end behav;
