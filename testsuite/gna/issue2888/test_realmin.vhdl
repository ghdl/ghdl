library IEEE;
use     IEEE.math_real.all;

entity test_Realmin is
end entity;

architecture test of test_Realmin is
  signal test_max : real := REALMAX(1.0, 2.0);  -- working
  signal test_min : real := REALMIN(1.0, 2.0);  -- unhandled call to function
begin
  proc: process
    variable min_value : real;
    begin
      min_value := REALMIN(1.0, 2.0);  -- working
      report "REALMIN exists and is: " & real'image(min_value);
      wait;
      std.env.finish;
  end process;
end architecture;
