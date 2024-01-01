library ieee;
use ieee.std_logic_1164.all;

entity test2 is
  port(
    clk_in: in std_logic;
    sig_1_in: in std_logic;
    sig_2_in: in integer range 0 to 15;
    sig_3_in: in std_logic
  );
end;

architecture formal of test2 is
begin
  default clock is rising_edge(clk_in);

  failing_assume: assume {
    sig_1_in = '1' and sig_2_in = 4 and sig_3_in = '0';
    sig_1_in = '1' and sig_2_in = 5 and sig_3_in = '0';
    sig_1_in = '0' and sig_2_in = 6 and sig_3_in = '0';
    sig_1_in = '1' and sig_2_in = 7 and sig_3_in = '0';
    sig_1_in = '1'};

end;
