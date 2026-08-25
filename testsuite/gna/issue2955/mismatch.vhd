library ieee; use ieee.std_logic_1164.all; use ieee.numeric_std.all;
use work.chk_pkg.all;

entity dut_mismatch is
  port (i_data : in  signed_vector(0 to 1);
        o_sum  : out signed(15 downto 0));
end entity;
architecture rtl of dut_mismatch is
begin
  o_sum <= resize(i_data(0), 16) + resize(i_data(1), 16);
end architecture;

library ieee; use ieee.std_logic_1164.all; use ieee.numeric_std.all;
use work.chk_pkg.all;
entity tb_mismatch is end entity;
architecture rtl of tb_mismatch is
  signal a : signed(15 downto 0) := to_signed(3, 16);
  signal b : signed(7 downto 0)  := to_signed(5, 8);
  signal s : signed(15 downto 0);
begin
  --  The element of signed_vector is unconstrained, so the two individual
  --  associations define it -- and they must define the same subtype.
  --  They do not: 16 bits and 8 bits.
  u : entity work.dut_mismatch
    port map (i_data(0) => a, i_data(1) => b, o_sum => s);
end architecture;
