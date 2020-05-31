library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.pkg.all;

entity foo is
  port (
    bus_in : in bus_t(data(7 downto 0));
    bus_out : out bus_t(data(7 downto 0))
  );
end foo;

architecture foo of foo is

begin

  bus_out <= bus_in;

end foo;
