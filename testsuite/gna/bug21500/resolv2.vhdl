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
  type arr2 is array (natural range <>) of rec1;

  function resolv (vec : arr2) return rec1
  is
  begin
    return vec (vec'left);
  end resolv;

  signal s : resolv rec1;
begin
end;
