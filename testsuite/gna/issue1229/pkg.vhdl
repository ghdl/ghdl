package pkg is
  generic (N : integer := 2);

  procedure showPackageN;

  impure function c_int return integer;
  attribute foreign of c_int : function is "VHPIDIRECT caux.so getInt";
end package pkg;

package body pkg is
  procedure showPackageN is
  begin
    report integer'image(N);
  end;

  impure function c_int return integer is
  begin
    assert false report "c_int VHPI" severity failure;
  end c_int;
end package body pkg;
