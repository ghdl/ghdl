package pkg is
  type protected_t is protected
  end protected protected_t;

  procedure proc(variable prot : inout protected_t; variable result : out boolean);
end package;

package body pkg is
  type protected_t is protected body
  end protected body protected_t;

  procedure proc(variable prot : inout protected_t; variable result : out boolean) is
  begin
  end;
end package body pkg;

use work.pkg.all;

package other_pkg is
  procedure other_proc(variable result : out boolean);
  alias other_proc is proc[protected_t, boolean];
  impure function other_proc return boolean;
end package;
