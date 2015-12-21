package pkg_FileIO is
    -------------------------------
    -- Define some basic data types
    -------------------------------
    subtype t_BYTE  is integer range 0 to 2**8 - 1;

    ---------------------------------------
    -- And arrays of those basic data types
    ---------------------------------------
    type arr_t_BYTE  is array(natural range <>) of t_BYTE;

    ----------------------------
    -- And a pointer to an array
    ----------------------------
    type ptr_arr_t_BYTE is access arr_t_BYTE;

    procedure Read_File(File_Name: in STRING; Data: inout ptr_arr_t_BYTE; Length: out integer);
end pkg_FileIO;

package body pkg_FileIO is
    procedure Read_File(File_Name: in STRING; Data: inout ptr_arr_t_BYTE; Length: out integer) is
    begin
        Data    := new arr_t_BYTE(0 to 10);
        for i in 0 to 10 loop
            Data(i) := 0; -- Comment this line out and GHDL is happy
        end loop;
        Length  := 11;
    end Read_File;
end pkg_FileIO;

