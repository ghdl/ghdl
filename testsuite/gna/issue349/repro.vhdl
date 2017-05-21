package ReproPkg is
  function resolved_max ( s : time_vector ) return time ;
  subtype  time_vector_max is (resolved_max) time_vector ;

  subtype integer2 is integer;
  
  function resolved_max ( s : integer_vector ) return integer ;
  subtype  integer_vector_max is (resolved_max) integer_vector ;
end package ReproPkg ;

package body ReproPkg is
  function resolved_max ( s : time_vector ) return time is
  begin
    return maximum(s) ;
  end function resolved_max ;

  function resolved_max ( s : integer_vector ) return integer is
  begin
    return maximum(s) ;
  end function resolved_max ;
end package body ReproPkg ;

use work.ReproPkg.all;

entity repro is
end;

architecture none of repro is
begin
end none;
