entity test is
end entity;

architecture tb of test is
begin
  process
  begin
    report to_string(3.0);
    wait;
  end process;
end architecture;
