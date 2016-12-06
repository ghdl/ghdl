library work;
    use work.all;

package ShiftReg is
    procedure main(new_sample: integer);
end package;

package body ShiftReg is
    procedure main(new_sample: integer) is
        variable dummy: Util.integer_list_t(0 to 3); -- Here i use the type
    begin
        dummy := new_sample & dummy(0 to dummy'high-1); -- Error about missing &
    end procedure;
end package body;
