use work.timing_pkg.all;

entity tb_to_string_overloading is
end entity;

architecture tb of tb_to_string_overloading is
  constant freq : frequency := 40.0e6;
begin

  process
  begin
    --report to_string(freq, "");
    report to_string(freq);
    wait;
  end process;

end architecture;
