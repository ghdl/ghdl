---------------------------------------------------------------------
---------------------------------------------------------------------
---                                                               ---
---             ____ _   _ ____  ____                             ---
---            / ___| \ | |  _ \/ ___|                            ---
---           | |   |  \| | |_) \___ \                            ---
---           | |___| |\  |  _ < ___) |                           ---
---            \____|_| \_|_| \_\____/                            ---
---            ____ _____    ___ _   _ ____  _   _                ---
---           |  _ \_   _|  |_ _| \ | / ___|| | | |               ---
---           | | | || |_____| ||  \| \___ \| | | |               ---
---           | |_| || |_____| || |\  |___) | |_| |               ---
---           |____/ |_|    |___|_| \_|____/ \___/                ---
---                                                               ---
---                                                               ---
---------------------------------------------------------------------
---------------------------------------------------------------------
library ieee;
    use ieee.std_logic_1164.all;

package pkg_Types is

    Constant c_classic     : std_ulogic_vector(1 downto 0) := "00"; --! classic cycle identifier
    Constant c_const_burst : std_ulogic_vector(1 downto 0) := "01"; --! Constant address burst cycle identifier
    Constant c_incr_burst  : std_ulogic_vector(1 downto 0) := "10"; --! incrementing burst cycle identifier
    Constant c_end_burst   : std_ulogic_vector(1 downto 0) := "11"; --! end of burst cycle identifier

    Type t_M2S_Base is 
    record
        cyc  : std_ulogic;
        cti  : std_ulogic_vector(1 downto 0);
        adr  : std_ulogic_vector;
        we   : std_ulogic;
        stb  : std_ulogic;
        dat  : std_ulogic_vector;
        sel  : std_ulogic_vector;
    end record;

    Type t_S2M_Base is 
    record
        ack : std_ulogic; 
        dat : std_ulogic_vector; 
        stl : std_ulogic;
        err : std_ulogic;
    end record;

    subtype t_M2S_B32 is 
        t_M2S_Base
        ( adr(open)
        , dat(31 downto 0)
        , sel( 3 downto 0)
        );

    subtype t_S2M_B32 is 
        t_S2M_base(dat(31 downto 0));   

end pkg_Types;