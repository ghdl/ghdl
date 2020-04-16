entity tb is
end entity tb;

architecture RTL of tb is
  package tbpkg is new work.pkg generic map (N => 3);
begin
  process
    use tbpkg.all;
    variable int : integer := c_int;
  begin
    report tbpkg.c_int'foreign;

    showPackageN;
    report "C Int: " & integer'image(int);
    wait;
  end process;
end architecture RTL;
