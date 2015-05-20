entity ent is
end entity;

architecture a of ent is
  procedure proc(bv : bit_vector) is
  begin
    report to_string(bv'length);
  end procedure;
begin
  main : process
    variable bv : bit_vector(0 to 1);
  begin
    proc(bv);
    wait;
  end process;
end architecture;
