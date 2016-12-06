package TestPkg is

  generic (
    G_TEST  : positive := 8
  ); 
end package TestPkg;


package body TestPkg is

  procedure TestReport is
  begin
    report "G_TEST :" & to_string(G_TEST);
  end procedure;

end package body;
