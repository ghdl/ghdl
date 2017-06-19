entity repro is
end;

architecture behav of repro is
  function f return natural is
  begin
    return 5;
  end f;

  constant cst : natural := f;

  type rec1 is record
    r : bit_vector (1 to cst);
  end record;

  type rec is record
    v : bit_vector;
    r : rec1;
  end record;

  procedure assign (signal s : out rec; val : rec) is
  begin
    s <= val;
  end assign;
begin
end behav;
