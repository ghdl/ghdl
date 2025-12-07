entity convrec02 is
  port (a: bit_vector(1 to 2);
        q: out bit_vector(0 to 1));
end;

architecture behav of convrec02 is
  type my_rec is record
    v : bit_vector(1 to 2);
  end record;
  signal s, s1 : my_rec;
begin
  s <= (v => a);
  s1 <= my_rec(s);
  q <= s1.v;
end;
