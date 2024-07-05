library ieee;
use ieee.std_logic_1164.all;

entity foo is
  port(
    a, b : in std_logic;
    clk  : in std_logic
  );
end foo;

architecture slow of foo is
  default clock is rising_edge(clk);
begin
  sequence s0 is {{a ; b[=2] ; a}[+] ; b ; a}; -- note "[=2]"
  sequence s1 is {s0 ; s0};
  assert (always s1);
end slow;
