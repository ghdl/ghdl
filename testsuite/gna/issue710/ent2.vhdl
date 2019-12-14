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
  use work.pkg.foo;
begin
  blk: block
    procedure foo is
    begin
      report "arch foo";
    end procedure;
  begin
    main : process
    begin
      foo;
    end process;
  end block;
end architecture;
