library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.pixel_pkg;

package pixel_column_pkg is
    generic(
        NBITS_IN                       : natural;
        NBR_OF_CHROMA_IN               : natural;
        NBR_OF_ROW_IN                  : natural;
        NBITS_OUT                      : natural;
        NBR_OF_CHROMA_OUT              : natural;
        NBR_OF_ROW_OUT                 : natural;
        package local_pixel_pkg is new pixel_pkg
            generic map (<>)
    );

    use local_pixel_pkg.all;

    type TYPE_PIXEL_COLUMN_IN is array (NBR_OF_ROW_IN-1 downto 0) of TYPE_PIXEL_IN;
    type TYPE_PIXEL_COLUMN_OUT is array (NBR_OF_ROW_OUT-1 downto 0) of TYPE_PIXEL_OUT;

    function std_logic_vector_to_pixel_column_in( in_vector : in std_logic_vector(NBR_OF_ROW_IN*NBR_OF_CHROMA_IN*NBITS_IN-1 downto 0))
        return TYPE_PIXEL_COLUMN_IN;

    function pixel_column_out_to_std_logic_vector( out_pixel_column : in TYPE_PIXEL_COLUMN_OUT)
        return std_logic_vector;

    function "not" (    pixel_column_in          : in TYPE_PIXEL_COLUMN_IN)
        return TYPE_PIXEL_COLUMN_OUT;


end package pixel_column_pkg;

package body pixel_column_pkg is

    function std_logic_vector_to_pixel_column_in( in_vector : in std_logic_vector(NBR_OF_ROW_IN*NBR_OF_CHROMA_IN*NBITS_IN-1 downto 0))
    return TYPE_PIXEL_COLUMN_IN is
        variable pixel_column_in : TYPE_PIXEL_COLUMN_IN; 
    begin
        for row in 0 to NBR_OF_ROW_IN -1 loop
            pixel_column_in(row) := std_logic_vector_to_pixel_in(in_vector((row+1)*NBR_OF_CHROMA_IN*NBITS_IN-1 downto row*NBR_OF_CHROMA_IN*NBITS_IN));
        end loop;
        return pixel_column_in;
    end function std_logic_vector_to_pixel_column_in;

    function pixel_column_out_to_std_logic_vector( out_pixel_column : in TYPE_PIXEL_COLUMN_OUT)
    return std_logic_vector is
        variable out_vector : std_logic_vector(NBR_OF_ROW_OUT*NBR_OF_CHROMA_OUT*NBITS_OUT-1 downto 0);
    begin
        for row in 0 to NBR_OF_ROW_OUT loop
            out_vector((row+1)*NBR_OF_CHROMA_IN*NBITS_IN-1 downto row*NBR_OF_CHROMA_IN*NBITS_IN) := pixel_out_to_std_logic_vector(out_pixel_column(row));
        end loop;
        return out_vector;
    end function pixel_column_out_to_std_logic_vector;

    function "not" (    pixel_column_in        : in TYPE_PIXEL_COLUMN_IN)
            return TYPE_PIXEL_COLUMN_OUT is
        variable pixel_column_out : TYPE_PIXEL_COLUMN_OUT;
    begin
        for index in pixel_column_in'range loop
            pixel_column_out(index) := not pixel_column_in(index);
        end loop;
        return pixel_column_out;
    end function "not";

end package body pixel_column_pkg;

