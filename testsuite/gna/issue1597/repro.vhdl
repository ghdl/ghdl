entity repro is
  port (b : bit_vector (7 downto 0));
end;

architecture behav of repro is
begin
  process
  begin
    for i in b'range loop
      if b(i)'active then
        report "active";
      end if;
    end loop;
    wait for 1 ns;
  end process;
end behav;
