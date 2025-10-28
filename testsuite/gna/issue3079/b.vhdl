library ieee;
use ieee.std_logic_1164.all;
use ieee.math_real.all;

entity A is
end A;

architecture sim of A is

  constant a1: natural := 256;
  constant a2: real := real(a1);
  constant b2: real := real(256);
  constant a3 : real := log2(a2);
  constant b3 : real :=log2(real(256)); 
  constant a4 : real := ceil(a3);
  constant b4 : real := ceil(log2(real(256)));
  constant a5 : natural := integer(a4);
  constant b5 : natural := integer(ceil(log2(real(256))));
begin
  process
  begin
    report "a1=" & natural'image(a1);
    report "a2=" & real'image(a2);
    report "a3=" & real'image(a3);
    report "a4=" & real'image(a4);
    report "a5=" & natural'image(a5);
    report "b5=" & natural'image(b5);
        wait for 10 ns;
        wait for 10 ns;
    wait;
  end process;
end architecture;
