package repro_pkg is
  type wishbone_type is record
    data : bit_vector;
    stb : bit;
  end record;
end;

entity reprob is
  port (b : bit_vector);
end;

architecture behav of reprob is
begin
  assert b'length = 4;
end;

use work.repro_pkg.all;

entity repro is
end repro;

architecture behav of repro is
  signal s : wishbone_type (data(3 downto 0));
begin
  dut: entity work.reprob
    port map (b => s.data);
end behav;
