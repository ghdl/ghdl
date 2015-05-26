entity ent is
end entity;

architecture a of ent is
begin
  main : process
    variable bv : bit_vector(0 to 0);
  begin
    report to_string(bv'length); -- Works
    report to_string(2 * bv'length); -- Does not work
    wait;
  end process;
end architecture;
