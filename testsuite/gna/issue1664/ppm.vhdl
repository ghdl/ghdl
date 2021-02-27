library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity ppm_cap is
	port (
		i_ppm 	: in std_logic;
		i_clk 	: in std_logic;
		i_rst 	: in std_logic;
		o_index	: out unsigned(3 downto 0);
		o_count	: out unsigned(15 downto 0);
		o_write	: out std_logic
	);
end ppm_cap;

architecture behavioral of ppm_cap is
	type state_type is (IDLE, A, B, WRITE_BACK);
	signal CurState, NextState : state_type;
	signal s_index 	: unsigned(3 downto 0);
	signal s_count 	: unsigned(15 downto 0);
	signal s_acc	: std_logic;
	signal s_wrout	: std_logic;

	signal s_wrcnt	: integer range 0 to 3;

begin

	o_index <= s_index;
	o_count <= s_count;
	o_write <= s_wrout;

	clk_proc : process(i_clk) begin
		if(i_rst = '1') then	
			CurState <= IDLE;
		else 
			if(s_acc = '1') then
				s_count <= s_count + 1;
			end if;
			if(CurState = WRITE_BACK) then
				s_wrcnt <= s_wrcnt + 1;
			end if;
			CurState <= NextState;
		end if;
	end process clk_proc;

	state_machine : process(CurState, i_ppm) begin 
		case CurState is
			when IDLE =>
				s_index <= x"0";
				s_count <= x"0000";
				s_acc <= '0';
				o_write <= '0';
				if(i_ppm = '0') then
					NextState <= A;
				else 
					NextState <= IDLE;
				end if;
			
			when A =>
				if(i_ppm = '1') then
					s_acc <= '1';
					NextState <= B;
				else 
					NextState <= A;
				end if;
			
			when B =>
				if(i_ppm = '0') then
					s_acc <= '0';
					NextState <= WRITE_BACK;
				end if;
			
			when WRITE_BACK =>
				if(s_wrcnt = 3 and s_index => 5) then
					s_index <= s_index + 1;
					NextState <= A;
				elsif(s_wrcnt = 3 and s_index = 5) then
					NextState <= IDLE;
				end if;
			when others =>
				NextState <= IDLE;
		end case;
	end process state_machine;
end behavioral; -- behavioral
