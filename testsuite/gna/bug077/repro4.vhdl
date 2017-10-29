entity repro3 is
end repro3;

architecture behav of repro3 is
  type my_rec is record
    a : bit;
    w : bit_vector;
  end record;

  procedure check (v : my_rec) is
  begin
    assert v = ('0', "10");
  end check;

  procedure pack (a : bit; w : bit_vector) is
  begin
    check (v.a => a,
           v.w => w);
  end pack;
begin
  process
  begin
    pack ('0', "01");
    wait;
  end process;
end;
