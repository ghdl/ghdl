package body other_pkg is
  procedure other_proc (
    variable result : out boolean) is
  begin
  end;

  impure function other_proc
    return boolean is
    variable result : boolean;
  begin
    return result;
  end function other_proc;
end package body other_pkg;
