package pkg is
  procedure proc;
end package;

package body pkg is
  procedure proc is
    type nested_prot_t is protected
      procedure proc;
    end protected;
    type nested_prot_t is protected body
      procedure proc is
      begin
      end procedure;
     end protected body;
     variable nested_prot : nested_prot_t;
  begin
    nested_prot.proc;
  end procedure;
end package body;

entity ent is
end;

architecture behav of ent is
begin
  work.pkg.proc;
end;

