entity repro2 is
end repro2;

architecture behav of repro2 is
begin

  process
    type my_rec is record
      inc : natural;
      b : bit;
    end record;

    constant bv : bit_vector := x"45";
    
    procedure proc (v : my_rec; bv : bit) is
    begin
      assert v.b = bv;
    end;
  begin
    proc (v => (inc => 3,
                b => bv(3)),
          bv => '0');
    wait;
  end process;
end behav;
