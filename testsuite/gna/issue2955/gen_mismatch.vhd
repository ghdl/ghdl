library ieee; use ieee.std_logic_1164.all; use ieee.numeric_std.all;
use work.chk_pkg.all;
entity dut_gen is
  generic (g_data : signed_vector(0 to 1));
end entity;
architecture rtl of dut_gen is
begin
  process begin
    report "g0 len=" & integer'image(g_data(0)'length)
         & " v=" & integer'image(to_integer(g_data(0)));
    report "g1 len=" & integer'image(g_data(1)'length)
         & " v=" & integer'image(to_integer(g_data(1)));
    wait;
  end process;
end architecture;

library ieee; use ieee.std_logic_1164.all; use ieee.numeric_std.all;
use work.chk_pkg.all;
entity tb_gen is end entity;
architecture rtl of tb_gen is
begin
  u : entity work.dut_gen
    --  The two individual associations do not define the same element
  --  subtype: 8 bits and 16 bits.  Without a check the second value is
  --  silently truncated to 8 bits and read back as 0.
  generic map (g_data(0) => to_signed(11, 8),
                 g_data(1) => to_signed(22, 16));
end architecture;
