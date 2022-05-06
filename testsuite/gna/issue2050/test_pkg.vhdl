package test_pkg is
    type rec_t is record
        comp_1 : integer;
    end record;

    constant REC_DEFAULT : rec_t := (comp_1 => 0);

    procedure proc(param : integer := REC_DEFAULT.comp_1);
end package;

package body test_pkg is
    procedure proc(param : integer := REC_DEFAULT.comp_1) is
    begin
        null;
    end procedure;
end package body;
