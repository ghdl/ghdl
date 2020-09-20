library ieee;
use ieee.std_logic_1164.all;
use ieee.float_pkg.all;

entity tb_division_float is
end;

architecture behav of tb_division_float is
  subtype my_float is float (7 downto -6);
  constant c0 : my_float := (others => '0');
  signal i0 : my_float := to_float(1.0, c0);
  signal i1 : my_float := to_float(2.0, c0);
  signal p0 : my_float;
begin
  dut: entity work.division_float
    port map (i0, i1, p0);

  process
  begin
    assert i0 = my_float'(b"0_0111111_000000");
    assert i1 = my_float'(b"0_1000000_000000");
    wait for 1 ns;
    assert p0 = my_float'(b"0_0111110_000000") severity failure;
    wait;
  end process;
end behav;
