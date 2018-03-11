entity repro_rec is
end;

architecture behav of repro_rec is
  type my_rec is record
    s : natural;
    b : bit_vector;
    c : bit_vector;
  end record;

  subtype my_rec1 is my_rec (c (2 to 3));
  
  signal r : my_rec1 (b (1 to 3));
  signal a : bit_vector (0 to 1);
begin
  process
  begin
    r.s <= 1;
    r.b <= "010";
    wait for 1 ns;
    r.b <= "101";
    wait;
  end process;

  blk: block
    port (a1 : bit_vector);
    port map (a1 => a);
  begin
  end block;
  
end;

    
