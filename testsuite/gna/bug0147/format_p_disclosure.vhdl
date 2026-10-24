entity format_p_disclosure is
end entity;

architecture test of format_p_disclosure is
begin
  process
    variable format_string : string (1 to 23);
    variable value : real;
  begin
    -- Constructed at run time to avoid frontend constant folding.
    format_string := "%p %p %p %p %p %p %p %p";
    value := 1.0;
    report to_string (value, format_string);
    wait;
  end process;
end architecture;

