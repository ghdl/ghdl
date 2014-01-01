entity repro is
end entity;

architecture ghdl_bug of repro is

begin
  --  Static
  assert bit_vector'("11100"  ror  5)  = "11100" report "ror  5 is broken" severity warning;
  --  Not static
  assert bit_vector'("11100") ror  5 = "11100" report string'("ror  5 is broken ")  severity warning;

  --  static
  assert bit_vector'("11100"  rol -5)  = "11100" report "rol -5 is broken" severity warning;
  --  not static
  assert bit_vector'("11100") rol -5 = "11100" report string'("rol -5 is broken ")  severity warning;
end architecture;
