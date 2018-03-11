library STD;
use STD.STANDARD.all;
use STD.TextIO;

package test is

  function IMAGE  (I  : Integer)          return string ;    

end package test;

package body test is

  function IMAGE(i : Integer) return string is
  variable l : TextIO.line ;
  variable s : string(1 to 80);
  variable r : Natural;
 begin
  TextIO.Write(l, i) ;
  r := l'length;
  TextIO.Read(l,s(1 to r));
  TextIO.Deallocate(l);
  return s(1 to r) ;
 end IMAGE  ;

end package body test;

