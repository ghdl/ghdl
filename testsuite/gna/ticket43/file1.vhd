package pkg is
  type rec_t is record
    boolean_field : boolean;
    integer_field : integer;
  end record;

  type other_rec_t is record
    integer_field : integer;
  end record;

  type protected_t is protected
  end protected protected_t;

  procedure proc(variable prot : inout protected_t; variable rec : inout rec_t);
  procedure proc(variable prot : inout protected_t; variable rec : inout other_rec_t);
end package;

package body pkg is
  type protected_t is protected body
  end protected body protected_t;

  procedure proc (variable prot : inout protected_t; variable rec : inout rec_t) is
  begin
  end;

  procedure proc(variable prot : inout protected_t; variable rec : inout other_rec_t) is
  begin
  end;
end package body;

use work.pkg.all;

package other_pkg is
  shared variable default_prot : protected_t;

  procedure other_proc(variable rec : inout rec_t);
  alias other_proc is proc[protected_t, rec_t];

  procedure other_proc(variable rec : inout other_rec_t);
  alias other_proc is proc[protected_t, other_rec_t];
end package;
