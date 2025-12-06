entity convrec01 is
end;

architecture behav of convrec01 is
  type my_rec is record
    v : bit_vector;
  end record;
begin
  process
    variable r1 : my_rec (v(0 to 3));
    variable r2 : my_rec (v(0 to 2));
  begin
    r2 := r1;
    wait;
  end process;
end;
