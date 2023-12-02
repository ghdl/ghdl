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

library work;
    use work.pkg_foo_Public.all;

entity foo is
    generic
    ( g_rst_pc    : std_ulogic_vector(32 - 1 downto 0) := 32X"0"
    ; g_rst_polar : std_ulogic                         := '1'
    ; g_sync_rst  : boolean                            := False
    );
    port
     ( Clk : in    std_ulogic
     ; Rst : in    std_ulogic

     ; M2S :   out t_foo_o
     ; S2M : in    t_foo_i
     );
end foo;
architecture rtl of foo is

    type t_reg 
    is 
    record
        pc  : std_ulogic_vector(32 - 1 downto 0);
        M2S : t_foo_o;
    end record;
    
    signal a
    ,      r
    : t_reg;

begin

    M2S <= r.M2S;
    
    P_Combi:
    process(All)
        variable v_a : t_reg;
    begin
        v_a := r;
        -- Mystères
        a <= v_a;
    end process;

    P_Reg:
    process(Clk, Rst)
    is
        procedure perform_reset 
        is
        begin
            r.M2S.cyc <= '0';
            r.M2S.stb <= '0';
            r.M2S.we  <= '0';
            r.M2S.adr <= (others => '0');
            r.M2S.dat <= (others => '0');
            r.M2S.sel <= (others => '0');
            r.M2S.cti <= (others => '0');         
        end procedure;
    begin
        if not(g_sync_rst) and (Rst = g_rst_polar) then
            perform_reset; 
        elsif rising_edge(Clk) then
            if g_sync_rst and (Rst = g_rst_polar) then
                perform_reset;
            else
                r <= a;
            end if;
        end if;
    end process;
end rtl;