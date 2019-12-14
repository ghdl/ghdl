library ieee;
  use ieee.std_logic_1164.all;

entity int_operators is
  generic (
    gen_a : integer := 5;
    gen_b : integer := 3
  );
  port (
    sig_a : in integer range 0 to 7;
    sig_b : out std_logic;
    sig_c : out integer
  );
end int_operators;

architecture rtl of int_operators is
begin
  sig_b <= '0' when sig_a /= gen_a else '1';
  sig_c <= gen_a rem gen_b;
end rtl;
