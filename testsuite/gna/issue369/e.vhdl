entity e is end entity;
architecture a of e is
  -- type type_t is range integer'low to integer'high; -- Unlimited range works fine
  type type_t is range 0 to integer'high; -- Limited range triggers CONSTRAINT_ERROR
  procedure p1(variable t :in    type_t) is begin             end procedure;
  procedure p2(variable t :  out type_t) is begin t :=     1; end procedure;
  procedure p3(variable t :inout type_t) is begin t := t + 1; end procedure;
begin
  process
    variable n :natural := 12;
  begin
    p1(        t  => type_t(n)); -- CONSTRAINT_ERROR
    p2(integer(t) =>        n );
    p3(integer(t) => type_t(n)); -- CONSTRAINT_ERROR
    wait;
  end process;
end architecture;
