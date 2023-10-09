package repro1_pkg is
  type my_rec is record
    v1 : bit_vector;
    v2 : bit_vector;
  end record;
end;

use work.repro1_pkg.all;

entity repro1_sub is
  generic (l : natural := 4);
  port (o : out my_rec (v1(open), v2(l -1 downto 0)));
end;

architecture behav of repro1_sub is
begin
  o.v2 <= b"0101";
end;

use work.repro1_pkg.all;

entity repro1 is
end;

architecture behav of repro1 is
  signal s : my_rec (v1(1 downto 0), v2(4 downto 1));
begin
  dut: entity work.repro1_sub port map (o => s);
end;

