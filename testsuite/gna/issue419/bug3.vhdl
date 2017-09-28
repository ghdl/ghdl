package pkg is
  procedure other_proc(value : boolean);
  procedure other_proc(value : integer);

  procedure proc;

end package;

package body pkg is
  procedure other_proc(value : boolean) is
  begin
    null;
  end;
  procedure other_proc(value : integer) is
  begin
    null;
  end;

  procedure proc is
  begin
    null;
  end;
  procedure proc2 is
  begin
    proc(other_proc => 0);
  end;
end package body;
