entity repro is
end;

architecture behav of repro is
  signal s : bit_vector(7 downto 0);
begin
  process (s)
  begin
    for i in s'range loop
      if s (i)'event then
        report "event for s bit " & natural'image (i);
      end if;
    end loop;
  end process;

  s <= x"42" after 1 ns, x"82" after 2 ns;
end behav;
