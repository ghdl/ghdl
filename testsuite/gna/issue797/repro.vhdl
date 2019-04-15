entity repro is
end;

architecture behav of repro is
  type byte_vector_access_t is access string;
  
  procedure set(index : natural; c : character) is
    variable v : byte_vector_access_t(1 to integer'high);
  begin
    v(index+1) := c;
  end;
begin
  process
  begin
    if now > 1 ns then
      set (1, 'a');
    end if;
    wait;
  end process;
end behav;

