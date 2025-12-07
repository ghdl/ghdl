library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity slice03 is
  port (raddr : std_logic_vector (3 downto 0);
        din : std_logic_vector(14 downto 7);
        rdat : out std_logic_vector(1 downto 0);
        rclk : std_logic);
end;

architecture behav of slice03 is
begin
  process (rclk)
    variable ra : natural;
  begin
    if rising_edge (rclk) then
      ra := to_integer(unsigned (raddr));
      rdat <= din (ra * 2 + 1 downto ra * 2);
    end if;
  end process;
end behav;
