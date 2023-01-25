entity st3 is
end;

architecture arch of st3 is
  type my_bv4 is array (natural range <>) of bit_vector(1 to 4);
  subtype mypos is natural range my_bv4'element'range;
  subtype my2 is mypos range 2 to 5;
begin
end arch;
