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

entity foo3 is
	generic
    (  g_rst_polar : bit := '1'
	 ; g_sync_rst  : boolean    := false
	 ; g_bug_mode  : boolean    := false
    );
    port
    ( i_clk : In 	bit
	; i_rst : In 	bit
	; i_bit : In	bit
	; o_bit :   Out	bit
    );
end foo3;
architecture behavioral of foo3 is

	type t_state is
	(  IDLE
	 , bar
	);

	type t_reg is record
		state : t_state;
		bit_o : bit;
	end record;

	Signal a
	, 	   r
	: t_reg;

begin

	o_bit <= r.bit_o;

	P_Combi:
	process(all)
	is
		variable v_a : t_reg;

		procedure Black_Magic;

		procedure handle_bit is
		begin
			if g_bug_mode then
				Black_Magic;
			end if;
			if i_bit = '1' then
				v_a.state := bar;
			end if;
		end handle_bit;

		procedure Black_Magic is
		begin
			v_a.bit_o := '0';
		end Black_Magic;
	begin
		v_a := r;
		case r.state is
			when IDLE =>
				handle_bit;
			when bar  =>
				v_a.bit_o := '1';
				v_a.state := IDLE;
		end case;
		a <= v_a;
	end process;

    P_Reg:
    process(i_clk, i_rst)
    is
        procedure perform_reset
        is
        begin
			r.bit_o <= '0';
			r.state <= IDLE;
        end procedure;
    begin
        if not(g_sync_rst) and (i_rst = g_rst_polar) then
            perform_reset;
        elsif rising_edge(i_clk) then
            if g_sync_rst and (i_rst = g_rst_polar) then
                perform_reset;
            else
                r <= a;
            end if;
        end if;
    end process;
end behavioral;
