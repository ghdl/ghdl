entity ent is
end entity;

architecture a of ent is
begin
  main : process
    variable t : time := 4 ns;
  begin
    report to_string(t / (2 ns));
    wait;
  end process;
end architecture;
