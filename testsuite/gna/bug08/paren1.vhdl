entity paren1 is
end paren1;

architecture behav of paren1
is
  signal a : bit_vector (1 to 4);
begin
  process
  begin
    for b in a'range loop
      assert a(b) = '0';
    end loop;
    wait;
  end process;
end behav;
