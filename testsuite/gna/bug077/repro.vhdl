package pkg is
  type my_inputs is record
    a : bit;
    w : bit_vector;
  end record;
end pkg;

use work.pkg.all;
entity child is
  port (i : my_inputs);
end;

architecture behav of child is
begin
  assert i.w = (i.w'range => i.a);
end behav;

entity repro is
end repro;

use work.pkg.all;
architecture behav of repro is
  signal s : bit_vector (7 downto 0);
  signal a : bit;
begin
  inst : entity work.child
    port map(
      i.a => a,
      i.w => s);

  process
  begin
    a <= '0';
    s <= x"01";
    wait;
  end process;
end;
