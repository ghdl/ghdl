library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity bar is
  port (
    clk      : in std_logic;
    input    : in unsigned(7 downto 0);
    output_a : out unsigned(7 downto 0);
    output_b : out unsigned(7 downto 0)
  );
end bar;

architecture bar of bar is

begin

  output_a <= 10 + input;
  -- This works as expected
  output_b <= input + 10;

end bar;
