library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package pixel_pkg is
    generic(
        NBITS_IN                       : natural;
        NBR_OF_CHROMA_IN               : natural;
        NBITS_OUT                      : natural;
        NBR_OF_CHROMA_OUT              : natural
    );

    subtype CHROMA_IN is unsigned(NBITS_IN-1 downto 0);
    type TYPE_PIXEL_IN is array (NBR_OF_CHROMA_IN-1 downto 0) of CHROMA_IN;
    
    subtype CHROMA_OUT is unsigned(NBITS_OUT-1 downto 0);
    type TYPE_PIXEL_OUT is array (NBR_OF_CHROMA_OUT-1 downto 0) of CHROMA_OUT;

    -- Note: every function is using unsigned vectors for now

    function std_logic_vector_to_pixel_in( in_vector : in std_logic_vector(NBR_OF_CHROMA_IN*NBITS_IN-1 downto 0))
        return TYPE_PIXEL_IN;

    function pixel_out_to_std_logic_vector( out_pixel : in TYPE_PIXEL_OUT)
        return std_logic_vector;

    function "not" (    pixel_in          : in TYPE_PIXEL_IN)
        return TYPE_PIXEL_OUT;





end package pixel_pkg;

package body pixel_pkg is

    function std_logic_vector_to_pixel_in( in_vector : in std_logic_vector(NBR_OF_CHROMA_IN*NBITS_IN-1 downto 0))
    return TYPE_PIXEL_IN is
        variable pixel_in : TYPE_PIXEL_IN; 
    begin
        for chroma in 0 to NBR_OF_CHROMA_IN -1 loop
            pixel_in(chroma) := unsigned(in_vector((chroma+1)*NBITS_IN-1 downto chroma*NBITS_IN));
        end loop;
        return pixel_in;
    end function std_logic_vector_to_pixel_in;

    function pixel_out_to_std_logic_vector( out_pixel : in TYPE_PIXEL_OUT)
    return std_logic_vector is
        variable out_vector : std_logic_vector(NBR_OF_CHROMA_OUT*NBITS_OUT-1 downto 0);
    begin
        for chroma in 0 to NBR_OF_CHROMA_OUT loop
            out_vector((chroma+1)*NBITS_OUT-1 downto chroma*NBITS_OUT) := std_logic_vector(out_pixel(chroma));
        end loop;
        return out_vector;
    end function pixel_out_to_std_logic_vector;

    function "not" (    pixel_in        : in TYPE_PIXEL_IN)
            return TYPE_PIXEL_OUT is
        variable pixel_out : TYPE_PIXEL_OUT;
    begin
        for index in pixel_in'range loop
            pixel_out(index) := not pixel_in(index);
        end loop;
        return pixel_out;
    end function "not";

end package body pixel_pkg;

