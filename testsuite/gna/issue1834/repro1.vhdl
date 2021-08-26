entity repro1 is
end;

architecture a of repro1 is
begin
  process
  begin
    -- Over int'high
    for i in integer'high to integer'high+1 loop
    end loop;

    -- Under int'low
    for i in integer'low downto integer'low-1 loop
    end loop;

    wait;
  end process;
end architecture;
