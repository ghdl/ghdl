library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test is
end entity test;
architecture beh of test is
  -- type t_unsigned_vector is array (natural range <>) of unsigned;
  type t_range_uns_vec is array (natural range <>) of t_unsigned_vector;
  procedure ptest(
    constant range_vec : in t_range_uns_vec) is
  begin
  end procedure;
begin
process(all)
  variable v_sig_long_min : unsigned(127 downto 0) := x"8F000000000000000000000000000000";
  variable v_sig_long_max : unsigned(127 downto 0) := x"8F000000000000000000000000000008";
  variable v_slv_long_min  : std_logic_vector(127 downto 0) := x"8F000000000000000000000000000000";
  variable v_slv_long_max  : std_logic_vector(127 downto 0) := x"8F000000000000000000000000000008";
begin
  ptest((0 => (v_sig_long_min, v_sig_long_max)));
  ptest((0 => (unsigned(v_slv_long_min), unsigned(v_slv_long_max))));
end process;
end architecture beh;
