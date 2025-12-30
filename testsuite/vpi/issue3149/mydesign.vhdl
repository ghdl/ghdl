library ieee ;
use ieee.std_logic_1164.all;

entity myentity is
  port (
    in1: in bit_vector(1 downto 0);
    in2 : in integer;
    in3 : real;
    in4 : natural;
    in5 : time;
    out1: out bit_vector(2 downto 0);
    inout1: inout bit_vector(3 downto 0);
    inout2: buffer bit_vector(1 to 3)
    );
end myentity;

architecture arch of myentity is
begin
end arch;
