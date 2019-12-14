package generic_pkg is
  generic (
    procedure generic_procedure
  );
end package;
package body generic_pkg is
  procedure internal_procedure is
  begin
    generic_procedure;
  end;
end package body;
