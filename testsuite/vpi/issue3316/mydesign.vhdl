library ieee;
use ieee.std_logic_1164.all;

entity myentity is
  port (
    sig_i : in  integer_vector(0 to 1) := (10, 20);
    sig_o : out integer_vector(0 to 1) := (11, 21)
  );
end entity myentity;

architecture arch of myentity is
begin
end architecture arch;
