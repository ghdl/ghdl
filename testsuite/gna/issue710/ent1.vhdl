package pkg is
  procedure foo;
end package;

package body pkg is
  procedure foo is
  begin
    report "pkg foo";
  end procedure;
end package body;

entity ent is
end entity;

architecture a of ent is
  procedure foo is
  begin
    report "arch foo";
  end procedure;
begin
  main : process
    use work.pkg.foo;
  begin
    foo; -- Causes ambiguity error but not if use clause is moved to architecture declarative region.
    wait;
  end process;
end architecture;
