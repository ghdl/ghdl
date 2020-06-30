library ieee;
use ieee.std_logic_1164.all;

entity cover3 is
  port (
    rst_n_i     : in    std_logic;
    clk_i       : in    std_logic;
    led0      : out   std_logic
    );
end cover3;

architecture top of cover3 is
  signal led : std_logic_vector(7 downto 0);
  -- Although not expected (there should be rising_edge), this was crashing.
  default clock is clk_i;
begin
  led0 <= led(0);

  cover { led(0) = '1' };
end architecture top;
