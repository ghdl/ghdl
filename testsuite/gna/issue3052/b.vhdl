entity B is
end;

architecture sim of b is
begin
  process
  begin
    report to_string(now);
    wait for 10 ns;
    report to_string(now);
    wait;
  end process;
end architecture;
