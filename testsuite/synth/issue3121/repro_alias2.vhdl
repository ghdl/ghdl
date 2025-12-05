package sub_pkg is
  type my_rec is record
    v : bit_vector(3 downto 0);
    a : bit;
  end record;
end sub_pkg;

entity sub is
  port (a : bit;
        o : out bit);
end sub;

use work.sub_pkg.all;

architecture behav of sub is
  signal t : bit;
  signal r : my_rec;
begin
  r.v <= (others => a);
  r.a <= not a;
  o <= r.v(1);
end behav;

entity repro is
  generic (gb: boolean := true);
  port (a : bit;
        b : out bit;
        c : out bit);
end repro;

use work.sub_pkg.all;

architecture behav of repro is
  alias t is << signal ^.dut.r.v : bit_vector>>;

  constant cb : bit := t(0);

  function is_x(v : bit) return boolean is
  begin
    return false;
  end is_x;
begin
  dut : entity work.sub port map (a, b);

  blk: block
  begin
  process(all)
  begin
    if not is_x(t(1)) then
      c <= t(2);
    end if;
  end process;
  end block;
end behav;
