entity format_n_analysis is
end entity;

architecture test of format_n_analysis is
begin
  process
  begin
    report to_string (1.0, "%n");
    wait;
  end process;
end architecture;
