library ieee; use ieee.std_logic_1164.all; use ieee.numeric_std.all;
use work.chk_pkg.all;

entity dut_dir is
  port (i_data : in  signed_vector(0 to 1);
        o_sum  : out signed(15 downto 0));
end entity;
architecture rtl of dut_dir is
begin
  o_sum <= resize(i_data(0), 16) + resize(i_data(1), 16);
end architecture;

library ieee; use ieee.std_logic_1164.all; use ieee.numeric_std.all;
use work.chk_pkg.all;
entity tb_dir is end entity;
architecture rtl of tb_dir is
  signal a : signed(15 downto 0) := to_signed(3, 16);
  signal b : signed(0 to 15)     := to_signed(5, 16);
  signal s : signed(15 downto 0);
begin
  --  Same length, opposite direction: legal, and it works.
  u : entity work.dut_dir
    port map (i_data(0) => a, i_data(1) => b, o_sum => s);

  process
  begin
    wait for 1 ns;
    assert s = to_signed(8, 16)
      report "FAIL s=" & integer'image(to_integer(s)) severity failure;
    report "PASS s=" & integer'image(to_integer(s));
    wait;
  end process;
end architecture;
