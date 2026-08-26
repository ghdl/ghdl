package p2 is
  type sv is array (natural range <>) of bit_vector;
  subtype nib  is bit_vector(3 downto 0);
  subtype byte is bit_vector(7 downto 0);
  function w8 (x : nib) return byte;
end package;

package body p2 is
  function w8 (x : nib) return byte is
  begin
    return x & x;
  end w8;
end package body;

use work.p2.all;
entity dut2 is
  generic (g : positive);
  port (o : out sv(0 to g-1));
end entity;
architecture a of dut2 is
begin
  o(0) <= "0100";
  o(1) <= "0100";
end architecture;

use work.p2.all;
entity tb_conv is end entity;
architecture a of tb_conv is
  signal y4 : nib;
  signal x8 : byte;
begin
  u : entity work.dut2 generic map (g => 2)
      port map (o(0) => y4, w8(o(1)) => x8);
  process begin
    wait for 1 ns;
    report "x8len=" & integer'image(x8'length) & " y4len=" & integer'image(y4'length);
    wait;
  end process;
end architecture;
