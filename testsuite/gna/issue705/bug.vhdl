package pkg is
  constant const : character := 'a';
end package;

entity tb_ent is
end entity;

architecture a of tb_ent is
  constant const : natural := 1;
begin
  main : process
    use work.pkg.const;
  begin
    report integer'image(const); -- 0 in GHDL, I expect 1
    assert const = 1; -- Fails in GHDL
  end process;
end architecture;
