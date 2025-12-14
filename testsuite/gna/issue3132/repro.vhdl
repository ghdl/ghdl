package pkg is
  function GetVhdlAssertCount return NATURAL ;
  function GetVhdlAssertCount (Level : SEVERITY_LEVEL ) return NATURAL ;

  type     AlertType                is (FAILURE, ERROR, WARNING) ;  -- NEVER
  subtype  AlertIndexType           is AlertType range FAILURE to WARNING ;
  type     AlertCountType           is array (AlertIndexType) of integer ;

  impure function GetVhdlAssertCount return AlertCountType ;
end;

package body pkg is
  function GetVhdlAssertCount return NATURAL is
  begin
    return 1;
  end;
  function GetVhdlAssertCount (Level : SEVERITY_LEVEL ) return NATURAL is
  begin
    return 2;
  end;
  impure function GetVhdlAssertCount return AlertCountType is
  begin
    return (3,4,5);
  end;
end;

use work.pkg.all;

entity repro is
end;

architecture behav of repro is
  procedure check(a, b : natural) is
  begin
    assert a = b severity failure;
  end check;

  procedure check(a, b : bit_vector) is
  begin
    assert a = b severity failure;
  end check;
begin
  check (GetVhdlAssertCount, 1);
  check (GetVhdlAssertCount (error), 2);
end;
