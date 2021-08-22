library ieee;
use ieee.std_logic_1164.all;

entity dut is
  generic (
    num_ports : integer
  );
  port (
    clocks : std_logic_vector(0 to num_ports - 1)
  );
end entity;

architecture a of dut is

begin

end architecture;
