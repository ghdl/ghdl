library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.math_real.all;

entity bug is
  port (
    dummy   : in std_ulogic
  );
end entity;

architecture rtl of bug is
	constant a : real := floor(15.0/4.0);
	constant b : real := ceil(15.0/4.0);
begin
end architecture;
