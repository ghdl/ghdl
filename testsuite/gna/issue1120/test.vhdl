library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test is
  generic(
    BITS  : positive;
    ZEROS : unsigned(BITS - 1 downto 0) := (others => '0'));
  port(
    min : in  u_unsigned(BITS - 1 downto 0) := ZEROS);
end entity;

architecture rtl of test is

begin

  process
    variable sum : unsigned(BITS - 2 downto 0);
    variable carry : std_ulogic;
  begin
    (carry, sum) := min;
    wait;
  end process;

end architecture;
