library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

library work;

entity math_real_test is
  port
  (
    dout          : out std_logic_vector(15 downto 0)
  );
end math_real_test;

architecture rtl of math_real_test is

  signal threshold        : std_logic_vector(15 downto 0) := std_logic_vector(to_unsigned(integer(floor(log10(sqrt(4096.0 * MATH_SQRT_PI)))), 16));

begin

   dout     <= threshold;

end;
