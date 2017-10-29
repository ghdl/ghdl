entity repro6 is
end repro6;

architecture behav of repro6 is
  type my_rec is record
    a : bit;
    w : bit_vector (1 to 3);
  end record;

  procedure check (signal v : my_rec) is
  begin
    assert v.a = '0' and v.w = "001";
  end check;

  procedure pack (signal a : bit; signal w : bit_vector) is
  begin
    check (v.a => a,
           v.w => w);
  end pack;

  signal sa : bit;
  signal sw : bit_vector (1 to 2);
begin
  process
  begin
    sa <= '0';
    sw <= "01";
    wait for 0 ns;
    pack (sa, sw);
    wait;
  end process;
end;
