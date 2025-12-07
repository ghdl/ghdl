library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity slice04 is
  port (raddr : std_logic_vector (3 downto 0);
        din : std_logic_vector(0 to 3);
        rdat : out std_logic_vector(0 to 1);
        rclk : std_logic);
end;

architecture behav of slice04 is
begin
  process (rclk)
    variable ra : integer;
  begin
    if rising_edge (rclk) then
      ra := to_integer(signed (raddr));
      rdat <= din(ra*2+2 to (ra + 1)*2 + 1);
    end if;
  end process;
end behav;
