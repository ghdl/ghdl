library ieee;
context ieee.ieee_std_context;
use ieee.math_real.all;
use ieee.numeric_std_unsigned.all;

entity mwe is
end entity;

architecture a of mwe is
begin
  process
    variable v_real : real := 10.0e6;
  begin
    report "some=" & to_string(25000000);
    report "some=" & to_string(25.0e6);
    report "some=" & to_string(real(v_real));
    wait;
  end process;
end;
