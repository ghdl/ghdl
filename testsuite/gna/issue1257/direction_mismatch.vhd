library ieee;
context ieee.ieee_std_context;
use ieee.math_real.all;

entity direction_mismatch is
    port (
        left : out std_logic_vector(1 to 0)
    );
end entity direction_mismatch;

architecture rtl of direction_mismatch is
    constant c : std_logic_vector(1 downto 0) := "01";
    -- function that can flip words around the center axis of an SLV
    -- ex. input=0x002_001, width=12, output=0x001_002
    -- ex. input=0x03_02_01, width=8, output=0x01_02_03
    function wordrevorder (
        arg   : std_logic_vector;
        width : positive
    )
    return std_logic_vector is
        constant c_ratio : integer := arg'length/width;
        variable v_ret : std_logic_vector(arg'range);
    begin
        for i in 0 to c_ratio-1 loop
            v_ret((i*width)+width-1 downto (i*width)) :=
                arg(((c_ratio-i-1)*width)+width-1 downto ((c_ratio-i-1)*width));
        end loop;
        return v_ret;
    end;
begin
    left <= wordrevorder(c, 1);
end architecture;