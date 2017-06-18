entity repro is
end;

architecture behav of repro is
  type rec is record
    v : bit_vector;
  end record;

  procedure assign (signal s : out rec; val : rec) is
  begin
    s <= val;
  end assign;
begin
end behav;
