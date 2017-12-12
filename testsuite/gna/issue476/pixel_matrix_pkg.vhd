library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.pixel_column_pkg;

package pixel_matrix_pkg is
    generic(
        NBITS_IN                       : natural;
        NBR_OF_CHROMA_IN               : natural;
        NBR_OF_ROW_IN                  : natural;
        NBR_OF_COL_IN                  : natural;
        NBITS_OUT                      : natural;
        NBR_OF_CHROMA_OUT              : natural;
        NBR_OF_ROW_OUT                 : natural;
        NBR_OF_COL_OUT                 : natural;
        package local_pixel_column_pkg is new pixel_column_pkg generic map (<>)
    );
    use local_pixel_column_pkg.all;

    type TYPE_PIXEL_MATRIX_IN is array (NBR_OF_COL_IN-1 downto 0) of TYPE_PIXEL_COLUMN_IN;
    type TYPE_PIXEL_MATRIX_OUT is array (NBR_OF_COL_OUT-1 downto 0) of TYPE_PIXEL_COLUMN_OUT;

    -- Note: this pkgs is used to propagate the functions that where defined for a (N x 1) array of pixel 
    --       to a (N x M) array of pixel

    function std_logic_vector_to_pixel_matrix_in( in_vector : in std_logic_vector(NBR_OF_COL_IN*NBR_OF_ROW_IN*NBR_OF_CHROMA_IN*NBITS_IN-1 downto 0))
        return TYPE_PIXEL_MATRIX_IN;

    function pixel_matrix_out_to_std_logic_vector( out_pixel_matrix : in TYPE_PIXEL_MATRIX_OUT)
        return std_logic_vector;


    function "not" (    pixel_matrix_in          : in TYPE_PIXEL_MATRIX_IN)
        return TYPE_PIXEL_MATRIX_OUT;



end package pixel_matrix_pkg;

package body pixel_matrix_pkg is

    function std_logic_vector_to_pixel_matrix_in( in_vector : in std_logic_vector(NBR_OF_COL_IN*NBR_OF_ROW_IN*NBR_OF_CHROMA_IN*NBITS_IN-1 downto 0))
    return TYPE_PIXEL_MATRIX_IN is
        variable pixel_matrix_in : TYPE_PIXEL_MATRIX_IN; 
    begin
        for col in 0 to NBR_OF_COL_IN-1 loop
            pixel_matrix_in(col) := std_logic_vector_to_pixel_column_in(in_vector((col+1)*NBR_OF_ROW_IN*NBR_OF_CHROMA_IN*NBITS_IN-1 downto col*NBR_OF_ROW_OUT*NBR_OF_CHROMA_IN*NBITS_IN));
        end loop;
        return pixel_matrix_in;
    end function std_logic_vector_to_pixel_matrix_in;

    function pixel_matrix_out_to_std_logic_vector( out_pixel_matrix : in TYPE_PIXEL_MATRIX_OUT)
    return std_logic_vector is
        variable out_vector : std_logic_vector(NBR_OF_COL_OUT*NBR_OF_ROW_OUT*NBR_OF_CHROMA_OUT*NBITS_OUT-1 downto 0);
    begin
        for col in 0 to NBR_OF_COL_OUT-1 loop
            out_vector(((col+1)*NBR_OF_ROW_IN*NBR_OF_CHROMA_IN)*NBITS_IN-1 downto col*NBR_OF_ROW_OUT*NBR_OF_CHROMA_IN*NBITS_IN) := pixel_column_out_to_std_logic_vector(out_pixel_matrix(col));
        end loop;
        return out_vector;
    end function pixel_matrix_out_to_std_logic_vector;

    function "not" (    pixel_matrix_in        : in TYPE_PIXEL_MATRIX_IN)
            return TYPE_PIXEL_MATRIX_OUT is
        variable pixel_matrix_out : TYPE_PIXEL_MATRIX_OUT;
    begin
        for index in 0 to NBR_OF_COL_IN-1 loop
            pixel_matrix_out(index) := not pixel_matrix_in(index);
        end loop;
        return pixel_matrix_out;
    end function "not";


end package body pixel_matrix_pkg;

