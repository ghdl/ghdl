library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity dpram7 is
  port (raddr : std_logic_vector (3 downto 0);
        rdat : out std_logic_vector (7 downto 0);
        waddr1 : std_logic_vector (3 downto 0);
        wdat1 : std_logic_vector (7 downto 0);
        waddr2 : std_logic_vector (3 downto 0);
        wdat2 : std_logic_vector (7 downto 0);
        rclk : std_logic;
        wclk : std_logic);
end;

architecture behav of dpram7 is
  subtype memtype is std_logic_vector (16 * 8 - 1 downto 0);
begin
  process (rclk, wclk)
    variable ra : natural;
    variable wa : natural;
    variable mem : memtype;
  begin
    if rising_edge(wclk) then
      wa := to_integer(unsigned (waddr1));
      mem (wa * 8 + 7 downto wa * 8) := wdat1;
      wa := to_integer(unsigned (waddr2));
      mem (wa * 8 + 7 downto wa * 8) := wdat2;
    end if;
    if rising_edge (rclk) then
      ra := to_integer(unsigned (raddr));
      rdat <= mem (ra * 8 + 7 downto ra * 8);
    end if;
  end process;
end behav;
