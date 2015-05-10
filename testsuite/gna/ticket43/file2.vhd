use work.pkg.all;

package body other_pkg is
  procedure other_proc(variable rec : inout rec_t) is
  begin
    proc(default_prot, rec);
  end;

  procedure other_proc(variable rec : inout other_rec_t) is
  begin
    proc(default_prot, rec);
  end;
end package body;
