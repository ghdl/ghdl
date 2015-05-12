package pkg is
  type rec0_t is record
    field0 : boolean;
  end record;

  type rec1_t is record
    field1 : boolean;
  end record;

  function fun(val : boolean) return rec0_t;
  function fun(val : boolean) return rec1_t;
  function fun(val : boolean) return boolean;

  procedure proc;
end package;

package body pkg is
  function fun(val : boolean) return rec0_t is
  begin
    return (field0 => val);
  end function;

  function fun(val : boolean) return rec1_t is
  begin
    return (field1 => val);
  end function;

  function fun(val : boolean) return boolean is
  begin
    return val;
  end function;

  procedure proc is
  begin
    assert fun(true).field0;
    assert fun(true).field1;
    assert fun(true);
  end procedure;

end package body;

entity ent is
end;

architecture behav of ent is
begin
  work.pkg.proc;
end behav;
  
