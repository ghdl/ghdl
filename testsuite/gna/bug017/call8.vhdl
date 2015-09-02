entity call8 is
end;

architecture behav of call8 is
  type int_vector is array (natural range <>) of integer;
  constant c : integer := 16#0123_4567#;
  
  procedure check (s : int_vector) is
  begin
    wait for 2 ns;
    assert s (2) = c;
  end;

  signal s : int_vector (0 to 3) := (123, 234, c, 345);
begin
  s (2) <= 456 after 1 ns;
  
  process
  begin
    check (s);
    report "SUCCESS" severity note;
    wait;
  end process;
 
end behav;
