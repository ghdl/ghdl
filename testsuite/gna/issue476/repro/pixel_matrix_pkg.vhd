library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.pixel_column_pkg;

package pixel_matrix_pkg is
    generic(
        NBITS                       : natural;
        NBR_OF_CHROMA               : natural;
        NBR_OF_ROW                  : natural;
        NBR_OF_COL                  : natural;
        package local_pixel_column_pkg is new pixel_column_pkg generic map (<>)
    );
    use local_pixel_column_pkg.all;

    type TYPE_PIXEL_MATRIX is array (NBR_OF_COL-1 downto 0) of TYPE_PIXEL_COLUMN;
    -- Note: this pkgs is used to propagate the functions that where defined for a (N x 1) array of pixel 
    --       to a (N x M) array of pixel

    function std_logic_vector_to_pixel_matrix( in_vector : in std_logic_vector(NBR_OF_COL*NBR_OF_ROW*NBR_OF_CHROMA*NBITS-1 downto 0))
        return TYPE_PIXEL_MATRIX;
end package pixel_matrix_pkg;

package body pixel_matrix_pkg is

    function std_logic_vector_to_pixel_matrix( in_vector : in std_logic_vector(NBR_OF_COL*NBR_OF_ROW*NBR_OF_CHROMA*NBITS-1 downto 0))
    return TYPE_PIXEL_MATRIX is
        variable pixel_matrix : TYPE_PIXEL_MATRIX; 
    begin
        for col in 0 to NBR_OF_COL-1 loop
            pixel_matrix(col) := std_logic_vector_to_pixel_column(in_vector((col+1)*NBR_OF_ROW*NBR_OF_CHROMA*NBITS-1 downto col*NBR_OF_ROW*NBR_OF_CHROMA*NBITS));
        end loop;
        return pixel_matrix;
    end function std_logic_vector_to_pixel_matrix;
end package body pixel_matrix_pkg;

