use work.gt2_dummy_pkg.all;

entity tb2 is
end;

architecture behav of tb2 is
  signal e : bit := '1';
  signal v : bit_vector(3 downto 0) := x"2";
begin
  slv_is_a_part_of_b(e, v);
end;
