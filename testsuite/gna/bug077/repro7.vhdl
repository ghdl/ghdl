entity repro7 is
end repro7;

architecture behav of repro7 is
  type my_rec is record
    a : bit;
    w : bit_vector;
  end record;

  procedure check (signal v : my_rec) is
  begin
    assert v.a = '0' and v.w = "01" severity failure;
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
