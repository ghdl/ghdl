ENTITY repro2 IS
END repro2;

architecture arch OF repro2 IS
  type my_rec is record
    bv : bit_vector;
    nat : natural;
  end record;

  type my_rec_array is array (natural range <>) of my_rec;

  constant cst : my_rec_array := (("01", 1), ("10", 2));
begin
  assert cst(0).nat = 1 severity failure;
  assert cst(1).nat = 2 severity failure;
end;
