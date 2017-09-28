package pkg is
  procedure other_proc(value : boolean);
  procedure other_proc(value : integer);

  procedure proc(other_proc : integer);
  procedure proc;

end package;

package body pkg is
  procedure proc2 is
  begin
    proc(other_proc => 0);
  end;
end package body;
