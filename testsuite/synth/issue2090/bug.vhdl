library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity bug is
port(
    clk : in std_ulogic
);
end entity;

library IEEE;
use IEEE.std_logic_1164.all;

entity ent is
generic(
    LEN : natural
);
port(
    data : in std_ulogic_vector(LEN-1 downto 0)
);
end entity;

architecture rtl of bug is

    constant ROWS       : natural := 5;
    constant COLS       : natural := 5;
    constant DATA_WIDTH : natural := 1;

    type data_t is record
        value : unsigned(DATA_WIDTH*8-1 downto 0);
    end record data_t;

    type table_t is array (0 to COLS-1, 0 to ROWS-1) of data_t;
    signal table : table_t;

    function table_to_sulv(table : table_t) return std_ulogic_vector is
		variable ret : std_ulogic_vector(COLS*ROWS*DATA_WIDTH*8-1 downto 0);
		variable idx : natural := 1;
	begin
		for y in 0 to ROWS-1 loop
			for x in 0 to COLS-1 loop
				ret(idx*8-1 downto (idx-1)*8) := std_ulogic_vector(table(x,y).value);
				idx := idx+1;
			end loop;
		end loop;
		return ret;
	end function;

begin
    u0 : entity work.ent 
    generic map(
        LEN => COLS*ROWS*DATA_WIDTH*8
    )
    port map(
        data => table_to_sulv(table)
    );
end architecture;

architecture rtl of ent is

begin

end architecture;
