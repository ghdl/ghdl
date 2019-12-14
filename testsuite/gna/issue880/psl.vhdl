entity psl is
end;

architecture behav of psl is
  signal a, b, c : bit;
  signal clk : bit;
  subtype wf_type is bit_vector (0 to 7);
  constant wave_a : wf_type := "10010100";
  constant wave_b : wf_type := "01001010";
  constant wave_c : wf_type := "00100101";
begin
  process
  begin
    for i in wf_type'range loop
      clk <= '0';
      wait for 1 ns;
      a <= wave_a (i);
      b <= wave_b (i);
      c <= wave_c (i);
      clk <= '1';
      wait for 1 ns;
    end loop;
    wait;
  end process;
  
  -- psl default clock is (clk'event and clk = '1');
  -- psl a1: assume always a |=> b;
  -- psl a2: assume always a -> eventually! c;
  -- psl c1: cover {a;b;c};
end behav;
