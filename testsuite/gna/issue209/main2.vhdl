library work;
    use work.all;

package ShiftReg is
    type integer_list_t is array (natural range <>) of integer; -- notice this line
    procedure main(new_sample: integer);
end package;

package body ShiftReg is

    procedure main(new_sample: integer) is
        variable dummy: integer_list_t(0 to 3); -- notice this line
    begin
        dummy := new_sample & dummy(0 to dummy'high-1); --no error
    end procedure;

end package body;
