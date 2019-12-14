----------------------------------------------------------------------------------
-- Author: Stefan Lohse
----------------------------------------------------------------------------------
library IEEE;
use			IEEE.std_logic_1164.all;

package utilities_pkg is
	type t_frequency is range 0 to natural'high units 
		Hz;												-- primary unit
		kHz = 1000 Hz;						-- secondary unit
		MHz = 1000 kHz;						-- secondary unit
		GHz = 1000 MHz;						-- secondary unit
	end units;
	
	function to_period(b: t_frequency) return time;
	function log2_ceil(a : integer) return natural;
	function isSimulation return boolean;
	
	-- ite = if-then-else
	function ite( cond: boolean; A: integer; B : integer) return integer;
	
	constant SIMULATION : boolean; -- deferred constant declaration
end package;

package body utilities_pkg is
	function to_period(b: t_frequency) return time is
		constant result : real := 1.0/real(t_frequency'pos(B));
	begin
		return result * 1 sec;
	end function;
	
	function log2_ceil(a : integer) return natural is
		variable pow2 : natural; -- equivalent zu := natural'left
	begin
		if a = 0 then
			return 1;
		end if;

		for i in 0 to 31 loop
			pow2 := 2**i;
			if pow2 > a then
				return i;
			end if;
		end loop;		
	end function;

	function isSimulation return boolean is
	begin
		return is_x('X');
	end function;
	
	constant SIMULATION : boolean := isSimulation;
	
	function ite( cond: boolean; A: integer; B : integer) return integer is
	begin
		if cond then
			return A;
		else
			return B;
		end if;
	end function;
end package body;


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.MATH_REAL.ALL;

use work.utilities_pkg.all;

package uart_pkg is
	type t_baudrate is range 0 to natural'high units -- oder (natural'low to natural'high) oder (1 to 1000000)
		Bd;
		kBd = 1000 Bd;
		MBd = 1000 kBd;
	end units;

	function to_period(b: t_baudrate) return time;
	function timing_to_cycles(period: time; frequency: t_frequency) return integer;
	function ite( cond: boolean; A: t_baudrate; B : t_baudrate) return t_baudrate;	
end package;

package body uart_pkg is
	function to_period(b: t_baudrate) return time is
		constant result : real := 1.0/real(t_baudrate'pos(B));
	begin
		return result * 1 sec;
	end function;

	function timing_to_cycles(period: time; frequency: t_frequency) return integer is
		variable res_real : real;
	begin
		res_real := real(time'pos(period)) / 1.0E12;
		res_real := real(t_frequency'pos(frequency)) * res_real;
	
		return integer(ceil(res_real));
	end function;

	function ite( cond: boolean; A: t_baudrate; B : t_baudrate) return t_baudrate is
	begin
		if cond then
			return A;
		else
			return B;
		end if;
	end function;
end package body;


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

use work.uart_pkg.ALL;
use work.utilities_pkg.all;

entity uart is
	generic ( 
		uart_oversampling  : positive := 16;
		num_of_databits    : positive := 8
	);
	port ( 
		clk                : in  STD_LOGIC;
		rst                : in  STD_LOGIC;
		rx                 : in  STD_LOGIC;
		valid              : out STD_LOGIC;
		error              : out STD_LOGIC;
		data               : out STD_LOGIC_VECTOR(num_of_databits-1 downto 0);
		uart_strobe        : in  STD_LOGIC
	);
end entity;

architecture rtl of uart is
	signal shift_reg      : std_logic_vector(num_of_databits-1 downto 0);
	signal shift_en       : std_logic;
	
	signal cntbit_value   : unsigned(log2ceil(num_of_databits) downto 0);
	signal cntbit_en      : std_logic;
	signal cntbit_rst     : std_logic;
	
	signal cntsamp_value  : unsigned(log2ceil(num_of_databits) downto 0);
	signal cntsamp_en     : std_logic;
	signal cntsamp_rst    : std_logic;
	
	type t_state is (s_idle, s_startbit, s_receive, s_stopbit, s_error);
	signal current_state  : t_state := s_idle;
	signal next_state     : t_state;
	
begin
	shift: process(clk)
	begin
		if rising_edge(clk) then
			if rst = '1' then
				shift_reg <= (others => '0');
			elsif shift_en = '1' then 
				shift_reg <= rx & shift_reg(shift_reg'right downto 1);
			end if;
		end if;
	end process;
	data <= shift_reg;
	
	cntbit: process(clk)
	begin
		if rising_edge(clk) then
			if cntbit_rst = '1' then
				cntbit_value <= (others => '0');
			elsif cntbit_en = '1' then 
				cntbit_value <= cntbit_value + 1;
			end if;
		end if;
	end process;
	
	cntsamp: process(clk)
	begin
		if rising_edge(clk) then
			if cntsamp_rst = '1' then
				cntsamp_value <= (others => '0');
			elsif cntsamp_en = '1' then 
				cntsamp_value <= cntsamp_value + 1;
			end if;
		end if;
	end process;
	
	-- FSM
	fsmreg: process(clk)
	begin
		if rising_edge(clk) then
			if rst = '1' then
				current_state <= s_idle;
			else 
				current_state <= next_state;
			end if;
		end if;
	end process;
	
	fsmcomb: process(current_state, uart_strobe, rx, cntbit_value, cntsamp_value)
	begin
		-- default values;
		next_state    <= current_state;
		valid         <= '0';
		error         <= '0';
		cntbit_en     <= '0';
		cntbit_rst    <= '0';
		cntsamp_en    <= '0';
		cntsamp_rst   <= '0';
		
		case current_state is
			when s_idle =>
				cntsamp_rst <= '1';
				cntbit_rst  <= '1';
				if rx = '0' then 
					next_state <= s_startbit;
				end if;
			
			when s_startbit =>
				cntsamp_en <= uart_strobe;
				if cntsamp_value = (uart_oversampling/2)-1 then
					cntsamp_rst <= '1';
					next_state  <= s_receive;
				end if;
				
			when s_receive =>
				cntsamp_en <= uart_strobe;
				if cntsamp_value = uart_oversampling-1 then
					cntsamp_rst <= '1';
					cntbit_en <= '1';
					shift_en  <= '1';
				end if;
				if cntbit_value = num_of_databits then
					cntsamp_rst <= '1';
					next_state <= s_stopbit;
				end if;			
				
			when s_stopbit =>
				cntsamp_en <= uart_strobe;
				valid      <= '1';
				if (cntsamp_value = 15) and rx = '1' then
					next_state <= s_idle;
				elsif (cntsamp_value = 15) and rx = '0' then
					next_state <= s_error;
				end if;
			
			when s_error =>
				error <= '1';
		end case;
	end process;
end architecture;


