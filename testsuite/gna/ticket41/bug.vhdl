package pkg is
  procedure proc;
  alias prog_alias is proc[];
end package;

package body pkg is
  procedure proc is
  begin
  end;

  impure function prog_alias return integer is
  begin
    prog_alias;
    return 0;
  end;
end package body;
