entity top is
  generic (width : natural := 8);
end top;

architecture behav of top is
  type arr1 is array (1 to width) of natural;
  type rec1 is record
    i : integer;
    a : arr1;
    c : character;
  end record;
  type arr2 is array (1 to width) of rec1;
  type arr3 is array (natural range <>) of arr2;

  function resolv (vec : arr3) return arr2
  is
  begin
    return vec (vec'left);
  end resolv;

  signal s : resolv arr2;
begin
end;


