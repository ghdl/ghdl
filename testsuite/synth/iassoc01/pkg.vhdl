package pkg is
  type nat_rec is record
    a, b : natural;
  end record;

  type nat_arr is array (natural range <>) of natural;
end pkg;
