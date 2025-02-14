library IEEE;
use     IEEE.math_real.all;

entity test_Realmax is
end entity;

architecture test of test_Realmax is
  signal test_max : real := REALMAX(1.0, 2.0);  -- working
begin
  proc: process
    variable max_value : real;
    begin
      max_value := REALMAX(1.0, 2.0);  -- working
      report "REALMAX exists and is: " & real'image(max_value);
      wait;
  end process;
end architecture;
