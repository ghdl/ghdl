package repro1_pkg is
  type rec1 is record
    a : bit;
    b : bit_vector;
  end record;

  type arr1_rec1 is array (natural range <>) of rec1;

  type arr2_rec1 is array (natural range <>) of rec1 (b(0 to 3));
end repro1_pkg;

use work.repro1_pkg.all;

entity repro1 is
end;

architecture behav of repro1 is
  signal s : arr2_rec1(1 to 2);
begin
  s(1).a <= '0';
end behav;

