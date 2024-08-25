use work.RandomPkg.RandomPType;

entity tb_bug is
end entity;

architecture tb of tb_bug is
begin
  main : process
    variable rnd : RandomPType;
    variable value : real;
  begin

    value := rnd.Uniform(0.0, 1.0);
    assert value >= 0.0 report "Got " & to_string(value) severity failure;
    
    wait;
  end process;
end architecture;
