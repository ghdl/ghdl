library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity test is
  port (
    Reset_n_i    : in  std_logic;
    Clk_i        : in  std_logic
  );
end entity test;


architecture beh of test is
begin

  default clock is rising_edge(Clk_i);
  restrict {Reset_n_i = '0'[*5]; Reset_n_i = '1'[+]}[*1];

end architecture beh;

