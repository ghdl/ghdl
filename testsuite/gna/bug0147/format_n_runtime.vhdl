entity format_n_runtime is
end entity;

architecture test of format_n_runtime is
begin
  process
    variable format_string : string (1 to 2);
    variable value : real;
  begin
    -- Constructed at run time to exercise the translated simulation path.
    format_string (1) := '%';
    format_string (2) := 'n';
    value := 1.0;
    report to_string (value, format_string);
    wait;
  end process;
end architecture;

