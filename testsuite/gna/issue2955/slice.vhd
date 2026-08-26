library ieee; use ieee.std_logic_1164.all; use ieee.numeric_std.all;
use work.chk_pkg.all;

entity dut_slice is
  port (i_data : in  signed_vector(0 to 1);
        o_sum  : out signed(15 downto 0));
end entity;
architecture rtl of dut_slice is
begin
  o_sum <= resize(i_data(0), 16) + resize(i_data(1), 16);
end architecture;

library ieee; use ieee.std_logic_1164.all; use ieee.numeric_std.all;
use work.chk_pkg.all;
entity tb_slice is end entity;
architecture rtl of tb_slice is
  signal aa : signed_vector(0 to 1)(15 downto 0)
    := (to_signed(3, 16), to_signed(5, 16));
  signal s  : signed(15 downto 0);
begin
  --  The formal part is a slice, so the actual is a slice of the array:
  --  what it gives is its own element subtype, not the array subtype.
  u : entity work.dut_slice port map (i_data(0 to 1) => aa, o_sum => s);

  process
  begin
    wait for 1 ns;
    assert s = to_signed(8, 16)
      report "FAIL s=" & integer'image(to_integer(s)) severity failure;
    report "PASS s=" & integer'image(to_integer(s));
    wait;
  end process;
end architecture;
