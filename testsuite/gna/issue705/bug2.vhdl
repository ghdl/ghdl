package pkg is
  constant const : natural := 0;
end package;

package pkg2 is
  constant const : natural := 2;
end package;

entity tb_ent is
end entity;

architecture a of tb_ent is
  use work.pkg.const;
begin
  main : process
    use work.pkg2.const;
  begin
    report integer'image(const); -- Should be an error as const is ambiguous
  end process;
end architecture;

