library ieee; use ieee.std_logic_1164.all; use ieee.numeric_std.all;
use work.chk_pkg.all;

entity dut_dyn is
  port (i_data : in  signed_vector(0 to 1);
        o_sum  : out signed(15 downto 0));
end entity;
architecture rtl of dut_dyn is
begin
  o_sum <= resize(i_data(0), 16) + resize(i_data(1), 16);
end architecture;

library ieee; use ieee.std_logic_1164.all; use ieee.numeric_std.all;
use work.chk_pkg.all;
entity tb_dyn is end entity;
architecture rtl of tb_dyn is
  --  Same mismatch as mismatch.vhd, but through a function call so that the
  --  bounds are not locally static and analysis cannot see it.
  function w (n : natural) return natural is
  begin
    return n;
  end function;
  constant c_w1 : natural := w(16);
  constant c_w2 : natural := w(8);
  signal a : signed(c_w1 - 1 downto 0) := to_signed(3, c_w1);
  signal b : signed(c_w2 - 1 downto 0) := to_signed(5, c_w2);
  signal s : signed(15 downto 0);
begin
  u : entity work.dut_dyn
    port map (i_data(0) => a, i_data(1) => b, o_sum => s);
end architecture;
