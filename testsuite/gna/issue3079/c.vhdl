library ieee;
use ieee.std_logic_1164.all;
use ieee.math_real.all;

entity A is
end A;

architecture sim of A is

  constant b_t_bits : integer := integer(ceil(log2(real(256))));
  subtype b_t is std_logic_vector(b_t_bits - 1 downto 0);
  -- subtype b_t is std_logic_vector(7 downto 0); -- works

begin
  process
  begin
   report "l=" & natural'image(b_t'left);
    wait;
  end process;
end architecture;
