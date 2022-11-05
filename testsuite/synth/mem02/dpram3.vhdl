library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity dpram3 is
  port (raddr : std_logic_vector (3 downto 0);
        rdat : out std_logic_vector (7 downto 0);
        waddr : std_logic_vector (3 downto 0);
        wdat : std_logic_vector (7 downto 0);
        rclk : std_logic;
        wclk : std_logic);
end;

architecture behav of dpram3 is
  subtype memtype is std_logic_vector (16 * 8 - 1 downto 0);
  signal mem : memtype;
begin
  process (rclk, wclk)
    variable ra : natural;
    variable wa : natural;
  begin
    if rising_edge (rclk) then
      ra := to_integer(unsigned (raddr));
      rdat <= mem (ra * 8 + 7 downto ra * 8);
    end if;
    if rising_edge(wclk) then
      wa := to_integer(unsigned (waddr));
      mem (wa * 8 + 7 downto wa * 8) <= wdat;
    end if;
  end process;
end behav;
