library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.pixel_pkg;

package pixel_column_pkg is
    generic(
        NBITS                       : natural;
        NBR_OF_CHROMA               : natural;
        NBR_OF_ROW                  : natural;
        package local_pixel_pkg is new pixel_pkg
            generic map (<>)
    );

    use local_pixel_pkg.all;

    type TYPE_PIXEL_COLUMN is array (NBR_OF_ROW-1 downto 0) of TYPE_PIXEL;

    function std_logic_vector_to_pixel_column
      (in_vector : in std_logic_vector(NBR_OF_ROW*NBR_OF_CHROMA*NBITS-1 downto 0))
        return TYPE_PIXEL_COLUMN;

end package pixel_column_pkg;

package body pixel_column_pkg is

    function std_logic_vector_to_pixel_column( in_vector : in std_logic_vector(NBR_OF_ROW*NBR_OF_CHROMA*NBITS-1 downto 0))
    return TYPE_PIXEL_COLUMN is
        variable pixel_column : TYPE_PIXEL_COLUMN; 
    begin
        for row in 0 to NBR_OF_ROW - 1 loop
            pixel_column(row) := std_logic_vector_to_pixel(in_vector((row+1)*NBR_OF_CHROMA*NBITS-1 downto row*NBR_OF_CHROMA*NBITS));
        end loop;
        return pixel_column;
    end function std_logic_vector_to_pixel_column;
end package body pixel_column_pkg;

