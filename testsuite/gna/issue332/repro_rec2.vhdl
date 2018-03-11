entity repro_rec is
end;

architecture behav of repro_rec is
  type my_rec is record
    s : natural;
    b : bit_vector;
  end record;

  signal r : my_rec (b (1 to 3));
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
    port (r1: my_rec; a1 : bit_vector);
    port map (r1 => r, a1 => a);
  begin
  end block;
  
end;

    
