library ieee; use ieee.std_logic_1164.all; use ieee.numeric_std.all;
package chk_pkg is
  type signed_vector is array(natural range <>) of signed;
end package;

library ieee; use ieee.std_logic_1164.all; use ieee.numeric_std.all;
use work.chk_pkg.all;
entity dut_chk is
  generic (g_bit_width : positive; g_num_chnl : positive);
  port (i_data : in  signed_vector(0 to g_num_chnl - 1);
        o_data : out signed_vector(0 to g_num_chnl - 1)(g_bit_width - 1 downto 0));
end entity;
architecture rtl of dut_chk is
begin
  o_data <= i_data;
end architecture;

library ieee; use ieee.std_logic_1164.all; use ieee.numeric_std.all;
use work.chk_pkg.all;
entity tb_chk2955 is end entity;
architecture rtl of tb_chk2955 is
  constant c_w : positive := 16;
  signal a, b : signed(c_w - 1 downto 0) := (others => '0');
begin
  --  Indexed formal association: the element subtype comes from the actual.
  u : entity work.dut_chk
    generic map (g_bit_width => c_w, g_num_chnl => 1)
    port map (i_data(0) => a, o_data(0) => b);

  process
  begin
    a <= to_signed(-12345, c_w); wait for 1 ns;
    assert b = to_signed(-12345, c_w)
      report "FAIL b=" & integer'image(to_integer(b)) severity failure;
    a <= to_signed(9876, c_w); wait for 1 ns;
    assert b = to_signed(9876, c_w)
      report "FAIL b=" & integer'image(to_integer(b)) severity failure;
    report "PASS b=" & integer'image(to_integer(b)) & " len=" & integer'image(b'length);
    wait;
  end process;
end architecture;
