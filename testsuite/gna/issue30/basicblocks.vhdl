library	ieee;
use		ieee.std_logic_1164.all;

package basicblocks_definitions is
constant	period:			time := 10 ns;
constant	half_period:	time := period / 2;
constant	reset_time:		time := 11 ns;
end;

library	ieee;
use		ieee.std_logic_1164.all;

library	work;
use		work.basicblocks_definitions.all;

-- FOR SIMULATION ONLY. NOT FOR PRODUCTION.

entity clkgen is
port(
	clk_out:	out	std_logic;
	resetn:		out	std_logic := '0'
);
end;

-- Tested 2016/01/19 with ghdl.  works.

architecture struct_clkgen of clkgen is

begin
	process begin

		resetn <= '1' after reset_time;

		clk_out <= '1';
		wait for half_period;

		clk_out <= '0';
		wait for half_period;
	end process;
end;

library	ieee;
use		ieee.std_logic_1164.all;

entity incrementer is
port(
	input:		in	std_logic;
	carry_in:	in	std_logic;
	sum:		out	std_logic;
	carry_out:	out	std_logic
);
end;

-- tested with ghdl for N = 16 starting 2016/01/22, finished ?, works.

architecture struct_incrementer of incrementer is
begin
	sum <= input xor carry_in;
	carry_out <= input and carry_in;
end;

library	ieee;
use		ieee.std_logic_1164.all;

entity andNbit is
generic(
	N:	positive
);
port(
	input:	in	std_logic_vector((N-1) downto 0);
	y:		out	std_logic
);
end;

-- Tested as part of counter testing.  Works.

architecture struct_andNbit of andNbit is

signal	and_vector:	std_logic_vector(N downto 0);

begin
	and_vector(0) <= '1';

	and_generator:
		for i in 1 to N generate
		and_vector(i) <= and_vector(i-1) and input(i-1);
	end generate and_generator;

	y <= and_vector(N);

end;

library	ieee;
use		ieee.std_logic_1164.all;

entity orNbit is
generic(
	N:	positive
);
port(
	input:	in	std_logic_vector((N-1) downto 0);
	y:		out	std_logic
);
end;

architecture struct_orNbit of orNbit is

signal	or_vector:	std_logic_vector(N downto 0);

begin
	or_vector(0) <= '0';

	or_generator:
		for i in 1 to N generate
		or_vector(i) <= or_vector(i-1) or input(i-1);
	end generate or_generator;

	y <= or_vector(N);

end;

library	ieee;
use		ieee.std_logic_1164.all;

entity incrementerN is
generic(
	N:	positive
);
port(
	input:		in	std_logic_vector((N-1) downto 0);
	carry_in:	in	std_logic;
	sum:		out	std_logic_vector((N-1) downto 0);
	carry_out:	out	std_logic
);
end;

-- tested with ghdl at N = 16 starting 2016/01/22, finished ?, works.

architecture struct_incrementerN of incrementerN is
component incrementer is
port(
	input:		in	std_logic;
	carry_in:	in	std_logic;
	sum:		out	std_logic;
	carry_out:	out	std_logic
);
end component;

signal	carry:	std_logic_vector(N downto 0);
signal	result:	std_logic_vector((N-1) downto 0);

begin
	carry(0) <= carry_in;

	u1: for i in (N-1) downto 0 generate
		u: incrementer port map(
			input => input(i),
			carry_in => carry(i),
			sum => result(i),
			carry_out => carry(i + 1)
		);
	end generate;

	carry_out <= carry(N);
	sum <= result;
end;

library	ieee;
use		ieee.std_logic_1164.all;

entity decoder1x16 is
port(
	data:		in	std_logic;
	y0:			out	std_logic;
	y1:			out	std_logic;
	y2:			out	std_logic;
	y3:			out	std_logic;
	y4:			out	std_logic;
	y5:			out	std_logic;
	y6:			out	std_logic;
	y7:			out	std_logic;
	y8:			out	std_logic;
	y9:			out	std_logic;
	y10:		out	std_logic;
	y11:		out	std_logic;
	y12:		out	std_logic;
	y13:		out	std_logic;
	y14:		out	std_logic;
	y15:		out	std_logic;
	address:	in	std_logic_vector(3 downto 0)
);
end;

architecture struct_decoder1x16 of decoder1x16 is
begin
	with address select	y0	<= data when x"0", '0' when others;
	with address select	y1	<= data when x"1", '0' when others;
	with address select	y2	<= data when x"2", '0' when others;
	with address select	y3	<= data when x"3", '0' when others;
	with address select	y4	<= data when x"4", '0' when others;
	with address select	y5	<= data when x"5", '0' when others;
	with address select	y6	<= data when x"6", '0' when others;
	with address select	y7	<= data when x"7", '0' when others;
	with address select	y8	<= data when x"8", '0' when others;
	with address select	y9	<= data when x"9", '0' when others;
	with address select	y10	<= data when x"a", '0' when others;
	with address select	y11	<= data when x"b", '0' when others;
	with address select	y12	<= data when x"c", '0' when others;
	with address select	y13	<= data when x"d", '0' when others;
	with address select	y14	<= data when x"e", '0' when others;
	with address select	y15	<= data when x"f", '0' when others;
end;

-- For reasons unknown, ghdl appears to ignore the generic definition of N in this and only
-- this architecture.  Error messages are generated at line 129 onwards.  No unusual characters
-- found in the file where the first error message is generated.

library	ieee;
use		ieee.std_logic_1164.all;

entity decoderNx16 is
generic(
	N:	positive
);
port(
	data:		in	std_logic_vector((N-1) downto 0);
	y0:			out	std_logic_vector((N-1) downto 0);
	y1:			out	std_logic_vector((N-1) downto 0);
	y2:			out	std_logic_vector((N-1) downto 0);
	y3:			out	std_logic_vector((N-1) downto 0);
	y4:			out	std_logic_vector((N-1) downto 0);
	y5:			out	std_logic_vector((N-1) downto 0);
	y6:			out	std_logic_vector((N-1) downto 0);
	y7:			out	std_logic_vector((N-1) downto 0);
	y8:			out	std_logic_vector((N-1) downto 0);
	y9:			out	std_logic_vector((N-1) downto 0);
	y10:		out	std_logic_vector((N-1) downto 0);
	y11:		out	std_logic_vector((N-1) downto 0);
	y12:		out	std_logic_vector((N-1) downto 0);
	y13:		out	std_logic_vector((N-1) downto 0);
	y14:		out	std_logic_vector((N-1) downto 0);
	y15:		out	std_logic_vector((N-1) downto 0);
	address:	in	std_logic_vector(3 downto 0)
);
end;

architecture struct_decoderNx16 of decoderNx16 is
component decoder1x16 is
port(
	data:		in	std_logic;
	y0:			out	std_logic;
	y1:			out	std_logic;
	y2:			out	std_logic;
	y3:			out	std_logic;
	y4:			out	std_logic;
	y5:			out	std_logic;
	y6:			out	std_logic;
	y7:			out	std_logic;
	y8:			out	std_logic;
	y9:			out	std_logic;
	y10:		out	std_logic;
	y11:		out	std_logic;
	y12:		out	std_logic;
	y13:		out	std_logic;
	y14:		out	std_logic;
	y15:		out	std_logic;
	address:	in	std_logic_vector(3 downto 0)
);
end component;

begin
	u1: for i in (N-1) downto 0 generate
		u: decoder1x16 port map(
			data => data(i),
			y0	=> y0(i),
			y1	=> y1(i),
			y2	=> y2(i),
			y3	=> y3(i),
			y4	=> y4(i),
			y5	=> y5(i),
			y6	=> y6(i),
			y7	=> y7(i),
			y8	=> y8(i),
			y9	=> y9(i),
			y10	=> y10(i),
			y11	=> y11(i),
			y12	=> y12(i),
			y13	=> y13(i),
			y14	=> y14(i),
			y15	=> y15(i),
			address => address
		);
	end generate u1;
end;

library	ieee;
use		ieee.std_logic_1164.all;

entity decoder1x2 is
port(
	data:		in	std_logic;
	selector:	in	std_logic;
	y0:			out	std_logic;
	y1:			out	std_logic
);
end;

-- Tested 2015/12/04 with Modelsim.  Works.

architecture struct_decoder1x2 of decoder1x2 is
begin
	with selector select y0 <= data when '0', '0' when others;
	with selector select y1 <= data when '1', '0' when others;
end;

library	ieee;
use		ieee.std_logic_1164.all;

entity decoderNx2 is
generic(
	N: positive
);
port(
	data:		in	std_logic_vector((N-1) downto 0);
	selector:	in	std_logic;
	y0:			out	std_logic_vector((N-1) downto 0);
	y1:			out	std_logic_vector((N-1) downto 0)
);
end;

-- tested 2015/12/27 at N = 8 with modelsim.  works.
-- tested 2016/01/23 at N = 8 with ghdl. works.

architecture struct_decoderNx2 of decoderNx2 is
component decoder1x2 is
port(
	data:		in	std_logic;
	selector:	in	std_logic;
	y0:			out	std_logic;
	y1:			out	std_logic
);
end component;

begin
	u1:	for i in (N-1) downto 0 generate
		u: decoder1x2 port map(
			data => data(i),
			selector => selector,
			y0 => y0(i),
			y1 => y1(i)
		);
	end generate u1;
end;

library	ieee;
use		ieee.std_logic_1164.all;

entity encoder2x1oe is
port(
	data0, data1:	in	std_logic;
	selector:		in	std_logic;
	enable:			in	std_logic;
	output:			out	std_logic
);
end;

-- tested during testing of encoder2xNoe, works.

architecture struct_encoder2x1oe of encoder2x1oe is
begin
	with selector select
		output <=	(data0 and enable) when '0',
					(data1 and enable) when '1',
					'0' when others;
end;

library	ieee;
use		ieee.std_logic_1164.all;

entity encoder2xN_oe is
generic(
	N:	positive
);
port(
	data0:		in	std_logic_vector((N-1) downto 0);
	data1:		in	std_logic_vector((N-1) downto 0);
	selector:	in	std_logic;
	enable:		in	std_logic;
	output:		out	std_logic_vector((N-1) downto 0)
);
end;

architecture struct_encoder2xN_oe of encoder2xN_oe is
component encoder2x1oe is
port(
	data0:		in	std_logic;
	data1:		in	std_logic;
	selector:	in	std_logic;
	enable:		in	std_logic;
	output:		out	std_logic
);
end component;

begin
	u1: for i in (N-1) downto 0 generate
		u: encoder2x1oe port map(
			data0 => data0(i),
			data1 => data1(i),
			selector => selector,
			enable => enable,
			output => output(i)
		);
	end generate u1;
end;

library	ieee;
use		ieee.std_logic_1164.all;

entity encoder2x1 is
port(
	data0:		in	std_logic;
	data1:		in	std_logic;
	selector:	in	std_logic;
	output:		out	std_logic
);
end;

-- tested during double register pair testing 2015/12/03.  Works.

architecture struct_encoder2x1 of encoder2x1 is
begin
	with selector select
		output <=	data0 when '0',
					data1 when '1',
					'0' when others;
end;

library	ieee;
use		ieee.std_logic_1164.all;

entity encoder2xN is
generic(
	N:	positive
);
port(
	data0:		in	std_logic_vector((N-1) downto 0);
	data1:		in	std_logic_vector((N-1) downto 0);
	selector:	in	std_logic;
	output:		out	std_logic_vector((N-1) downto 0)
);
end;

-- tested during double register pair testing 2015/12/03.  Works for N = 8.
-- also tested during alu testing.  works there too.

architecture struct_encoder2xN of encoder2xN is
component encoder2x1 is
port(
	data0:		in	std_logic;
	data1:		in	std_logic;
	selector:	in	std_logic;
	output:		out	std_logic
);
end component;

begin
	u1: for i in (N-1) downto 0 generate
		u: encoder2x1 port map(
			data0 => data0(i),
			data1 => data1(i),
			selector => selector,
			output => output(i)
		);
	end generate u1;
end;

library	ieee;
use		ieee.std_logic_1164.all;

entity synchronous_latch is
port(
	rstn:			in	std_logic;
	clock:			in	std_logic;
	clock_enable:	in	std_logic;
	d:				in	std_logic;
	q:				out	std_logic
);
end;

-- Tested 2016/11/21, works on Modelsim simulator.

architecture struct_synchronous_latch of synchronous_latch is
begin
	process(rstn, clock, clock_enable)
	variable	datum:	std_logic;
	begin
		if rstn = '0' then
			datum := '0';
		elsif rising_edge(clock) then
			if clock_enable = '1' then
				datum := d;
			end if;
		end if;

		q <= datum;
	end process;
end;

library	ieee;
use		ieee.std_logic_1164.all;

-- library	altera;
-- use		altera.altera_primitives_components.all;

entity synchronous_latchN is
generic(
	N:	positive
);
port(
	rstn:			in	std_logic;
	clock:			in	std_logic;
	clock_enable:	in	std_logic;
	d:				in	std_logic_vector((N-1) downto 0);
	q:				out	std_logic_vector((N-1) downto 0)
);
end;

-- Tested 2016/11/21, works on Modelsim simulator.

architecture struct_synchronous_latchN of synchronous_latchN is
component synchronous_latch is
port(
	rstn:			in	std_logic;
	clock:			in	std_logic;
	clock_enable:	in	std_logic;
	d:				in	std_logic;
	q:				out	std_logic
);
end component;

begin
	u1:	for i in 0 to (N-1) generate
		u: synchronous_latch port map(
			rstn => rstn,
			clock => clock,
			clock_enable => clock_enable,
			d => d(i),
			q => q(i)
		);
	end generate u1;
end;

library	ieee;
use		ieee.std_logic_1164.all;

entity synchronous_latch_oe is
port(
	rstn:			in	std_logic;
	clock:			in	std_logic;
	clock_enable:	in	std_logic;
	oe:				in	std_logic;
	d:				in	std_logic;
	q:				out	std_logic
);
end;

-- tested 2015/12/27 as part of testing synchronous_latchN_oe.  works.

architecture struct_synchronous_latch_oe of synchronous_latch_oe is
begin
	process(rstn, clock, clock_enable, oe)
	variable	datum:	std_logic;
	begin
		if rstn = '0' then
			datum := '0';
		elsif rising_edge(clock) then
			if clock_enable = '1' then
				datum := d;
			end if;
		end if;

		if oe = '1' then
			q <= datum;
		else
			q <= 'Z';
		end if;
	end process;
end;

library	ieee;
use		ieee.std_logic_1164.all;

entity synchronous_latchN_oe is
generic(
	N: positive
);
port(
	rstn:			in	std_logic;
	clock:			in	std_logic;
	clock_enable:	in	std_logic;
	oe:				in	std_logic;
	d:				in	std_logic_vector((N-1) downto 0);
	q:				out	std_logic_vector((N-1) downto 0)
);
end;

-- tested 2015/12/27, N = 8 with modelsim.  works.

architecture struct_synchronous_latchN_oe of synchronous_latchN_oe is
component synchronous_latch_oe is
port(
	rstn:			in	std_logic;
	clock:			in	std_logic;
	clock_enable:	in	std_logic;
	oe:				in	std_logic;
	d:				in	std_logic;
	q:				out	std_logic
);
end component;

begin
	u1:	for i in (N-1) downto 0 generate
		u: synchronous_latch_oe port map(
			rstn => rstn,
			clock => clock,
			clock_enable => clock_enable,
			oe => oe,
			d => d(i),
			q => q(i)
		);
	end generate u1;
end;

library	ieee;
use		ieee.std_logic_1164.all;

entity synchronous_latch_autoclear is
port(
	rstn:			in	std_logic;
	clock:			in	std_logic;
	clock_enable:	in	std_logic;
	d:				in	std_logic;
	q:				out	std_logic
);
end;

-- Tested 2016/11/21, works on Modelsim simulator.

architecture struct_synchronous_latch_autoclear of synchronous_latch_autoclear is
begin
	process(rstn, clock)
	variable	datum:	std_logic;
	begin
		if rstn = '0' then
			datum := '0';
		elsif rising_edge(clock) then
			if clock_enable = '1' then
				if datum = '1' then
					datum := '0';
				else
					datum := d;
				end if;
			else
				datum := '0';
			end if;
		end if;

		q <= datum;
	end process;
end;

library	ieee;
use		ieee.std_logic_1164.all;

entity encoder4x1 is
port(
	data0:		in  std_logic;
	data1:		in  std_logic;
	data2:		in  std_logic;
	data3:		in  std_logic;
	address:	in  std_logic_vector(1 downto 0);
	output:		out std_logic
	);
end;

-- tested 2015/12/26 with modelsim as part of encoder4xN, works.

architecture struct_encoder4x1 of encoder4x1 is
begin
	with address select
		output <=	data0 when "00",
					data1 when "01",
					data2 when "10",
					data3 when "11",
					'0' when others;
end;

library	ieee;
use		ieee.std_logic_1164.all;

entity encoder4xN is
generic(
	N:	positive
);
port(
	data0:		in  std_logic_vector((N-1) downto 0);
	data1:		in  std_logic_vector((N-1) downto 0);
	data2:		in  std_logic_vector((N-1) downto 0);
	data3:		in  std_logic_vector((N-1) downto 0);
	address:	in  std_logic_vector(1 downto 0);
	output:		out std_logic_vector((N-1) downto 0)
	);
end;

-- tested 2015/12/26 with modelsim at N = 16, works.

architecture struct_encoder4xN of encoder4xN is
component encoder4x1 is
port(
	data0:		in  std_logic;
	data1:		in  std_logic;
	data2:		in  std_logic;
	data3:		in  std_logic;
	address:	in  std_logic_vector(1 downto 0);
	output:		out std_logic
	);
end component;

begin
	u1: for i in (N-1) downto 0 generate
		u: encoder4x1 port map(
			data0 => data0(i),
			data1 => data1(i),
			data2 => data2(i),
			data3 => data3(i),
			address => address,
			output => output(i)
		);
	end generate u1;
end;

library	ieee;
use		ieee.std_logic_1164.all;

entity encoder8x1 is
port(
	data0:		in	std_logic;
	data1:		in	std_logic;
	data2:		in	std_logic;
	data3:		in	std_logic;
	data4:		in	std_logic;
	data5:		in	std_logic;
	data6:		in	std_logic;
	data7:		in	std_logic;
	address:	in	std_logic_vector(2 downto 0);
	output:		out	std_logic
);
end;

-- tested 2015/12/26 as part of encoder8xN with modelsim, works.

architecture struct_encoder8x1 of encoder8x1 is
begin
	with address select
		output <=	data0 when "000",
					data1 when "001",
					data2 when "010",
					data3 when "011",
					data4 when "100",
					data5 when "101",
					data6 when "110",
					data7 when "111",
					'0' when others;
end;

library	ieee;
use		ieee.std_logic_1164.all;

entity encoder8xN is
generic(
	N:	positive
);
port(
	data0:		in	std_logic_vector((N-1) downto 0);
	data1:		in	std_logic_vector((N-1) downto 0);
	data2:		in	std_logic_vector((N-1) downto 0);
	data3:		in	std_logic_vector((N-1) downto 0);
	data4:		in	std_logic_vector((N-1) downto 0);
	data5:		in	std_logic_vector((N-1) downto 0);
	data6:		in	std_logic_vector((N-1) downto 0);
	data7:		in	std_logic_vector((N-1) downto 0);
	address:	in	std_logic_vector(2 downto 0);
	output:		out	std_logic_vector((N-1) downto 0)
);
end;

-- tested 2015/12/26 for N = 8 with modelsim, works.

architecture struct_encoder8xN of encoder8xN is
component encoder8x1 is
port(
	data0:		in	std_logic;
	data1:		in	std_logic;
	data2:		in	std_logic;
	data3:		in	std_logic;
	data4:		in	std_logic;
	data5:		in	std_logic;
	data6:		in	std_logic;
	data7:		in	std_logic;
	address:	in	std_logic_vector(2 downto 0);
	output:		out	std_logic
);
end component;

begin
	u1: for i in (N-1) downto 0 generate
		u: encoder8x1 port map(
			data0 => data0(i),
			data1 => data1(i),
			data2 => data2(i),
			data3 => data3(i),
			data4 => data4(i),
			data5 => data5(i),
			data6 => data6(i),
			data7 => data7(i),
			address => address,
			output => output(i)
		);
	end generate u1;
end;

library	ieee;
use		ieee.std_logic_1164.all;

entity decoder1x8 is
port(
	data:		in	std_logic;
	y0:			out	std_logic;
	y1:			out	std_logic;
	y2:			out	std_logic;
	y3:			out	std_logic;
	y4:			out	std_logic;
	y5:			out	std_logic;
	y6:			out	std_logic;
	y7:			out	std_logic;
	address:	in	std_logic_vector(2 downto 0)
);
end;

-- tested 2015/12/30 with modelsim as part of decoderNx8, works.

architecture struct_decoder1x8 of decoder1x8 is
begin
	with address select y0	<= data when "000", '0' when others;
	with address select y1	<= data when "001", '0' when others;
	with address select y2	<= data when "010", '0' when others;
	with address select y3	<= data when "011", '0' when others;
	with address select y4	<= data when "100", '0' when others;
	with address select y5	<= data when "101", '0' when others;
	with address select y6	<= data when "110", '0' when others;
	with address select y7	<= data when "111", '0' when others;
end;

library	ieee;
use		ieee.std_logic_1164.all;

entity decoderNx8 is
generic(
	N:	positive
);
port(
	data:		in	std_logic_vector((N-1) downto 0);
	y0:			out	std_logic_vector((N-1) downto 0);
	y1:			out	std_logic_vector((N-1) downto 0);
	y2:			out	std_logic_vector((N-1) downto 0);
	y3:			out	std_logic_vector((N-1) downto 0);
	y4:			out	std_logic_vector((N-1) downto 0);
	y5:			out	std_logic_vector((N-1) downto 0);
	y6:			out	std_logic_vector((N-1) downto 0);
	y7:			out	std_logic_vector((N-1) downto 0);
	address:	in	std_logic_vector(2 downto 0)
);
end;

-- tested 2015/12/30 with modelsim, works.

architecture struct_decoderNx8 of decoderNx8 is
component decoder1x8 is
port(
	data:		in	std_logic;
	y0:			out	std_logic;
	y1:			out	std_logic;
	y2:			out	std_logic;
	y3:			out	std_logic;
	y4:			out	std_logic;
	y5:			out	std_logic;
	y6:			out	std_logic;
	y7:			out	std_logic;
	address:	in	std_logic_vector(2 downto 0)
);
end component;

begin
	u1: for i in (N-1) downto 0 generate
		u: decoder1x8 port map(
			data => data(i),
			y0	=> y0(i),
			y1	=> y1(i),
			y2	=> y2(i),
			y3	=> y3(i),
			y4	=> y4(i),
			y5	=> y5(i),
			y6	=> y6(i),
			y7	=> y7(i),
			address=> address
		);
	end generate u1;
end;

library	ieee;
use		ieee.std_logic_1164.all;

entity encoder16x1 is
port(
	data0:		in	std_logic;
	data1:		in	std_logic;
	data2:		in	std_logic;
	data3:		in	std_logic;
	data4:		in	std_logic;
	data5:		in	std_logic;
	data6:		in	std_logic;
	data7:		in	std_logic;
	data8:		in	std_logic;
	data9:		in	std_logic;
	data10:		in	std_logic;
	data11:		in	std_logic;
	data12:		in	std_logic;
	data13:		in	std_logic;
	data14:		in	std_logic;
	data15:		in	std_logic;
	address:	in	std_logic_vector(3 downto 0);
	output:		out	std_logic
);
end;

-- tested with Modelsim 2015/12/24 as part of encoder16xN, works with N = 8.

architecture struct_encoder16x1 of encoder16x1 is
begin
	with address select
		output <=	data0	when	"0000",
					data1	when	"0001",
					data2	when	"0010",
					data3	when	"0011",
					data4	when	"0100",
					data5	when	"0101",
					data6	when	"0110",
					data7	when	"0111",
					data8	when	"1000",
					data9	when	"1001",
					data10	when	"1010",
					data11	when	"1011",
					data12	when	"1100",
					data13	when	"1101",
					data14	when	"1110",
					data15	when	"1111",
					'0' when others;
end;

library	ieee;
use		ieee.std_logic_1164.all;

entity encoder16xN is
generic(
	N:	positive
);
port(
	data0:		in	std_logic_vector((N-1) downto 0);
	data1:		in	std_logic_vector((N-1) downto 0);
	data2:		in	std_logic_vector((N-1) downto 0);
	data3:		in	std_logic_vector((N-1) downto 0);
	data4:		in	std_logic_vector((N-1) downto 0);
	data5:		in	std_logic_vector((N-1) downto 0);
	data6:		in	std_logic_vector((N-1) downto 0);
	data7:		in	std_logic_vector((N-1) downto 0);
	data8:		in	std_logic_vector((N-1) downto 0);
	data9:		in	std_logic_vector((N-1) downto 0);
	data10:		in	std_logic_vector((N-1) downto 0);
	data11:		in	std_logic_vector((N-1) downto 0);
	data12:		in	std_logic_vector((N-1) downto 0);
	data13:		in	std_logic_vector((N-1) downto 0);
	data14:		in	std_logic_vector((N-1) downto 0);
	data15:		in	std_logic_vector((N-1) downto 0);
	address:	in	std_logic_vector(3 downto 0);
	output:		out	std_logic_vector((N-1) downto 0)
);
end;

-- tested with Modelsim 2015/12/24, works with N = 8.

architecture struct_encoder16xN of encoder16xN is
component encoder16x1 is
port(
	data0:		in	std_logic;
	data1:		in	std_logic;
	data2:		in	std_logic;
	data3:		in	std_logic;
	data4:		in	std_logic;
	data5:		in	std_logic;
	data6:		in	std_logic;
	data7:		in	std_logic;
	data8:		in	std_logic;
	data9:		in	std_logic;
	data10:		in	std_logic;
	data11:		in	std_logic;
	data12:		in	std_logic;
	data13:		in	std_logic;
	data14:		in	std_logic;
	data15:		in	std_logic;
	address:	in	std_logic_vector(3 downto 0);
	output:		out	std_logic
);
end component;

begin
	u1: for i in (N-1) downto 0 generate
		u: encoder16x1 port map(
				data0 => data0(i),
				data1 => data1(i),
				data2 => data2(i),
				data3 => data3(i),
				data4 => data4(i),
				data5 => data5(i),
				data6 => data6(i),
				data7 => data7(i),
				data8 => data8(i),
				data9 => data9(i),
				data10 => data10(i),
				data11 => data11(i),
				data12 => data12(i),
				data13 => data13(i),
				data14 => data14(i),
				data15 => data15(i),
				address => address,
				output => output(i)
			);
	end generate u1;
end;

library	ieee;
use		ieee.std_logic_1164.all;

entity encoder32x1 is
port(
	data0:		in	std_logic;
	data1:		in	std_logic;
	data2:		in	std_logic;
	data3:		in	std_logic;
	data4:		in	std_logic;
	data5:		in	std_logic;
	data6:		in	std_logic;
	data7:		in	std_logic;
	data8:		in	std_logic;
	data9:		in	std_logic;
	data10:		in	std_logic;
	data11:		in	std_logic;
	data12:		in	std_logic;
	data13:		in	std_logic;
	data14:		in	std_logic;
	data15:		in	std_logic;
	data16:		in	std_logic;
	data17:		in	std_logic;
	data18:		in	std_logic;
	data19:		in	std_logic;
	data20:		in	std_logic;
	data21:		in	std_logic;
	data22:		in	std_logic;
	data23:		in	std_logic;
	data24:		in	std_logic;
	data25:		in	std_logic;
	data26:		in	std_logic;
	data27:		in	std_logic;
	data28:		in	std_logic;
	data29:		in	std_logic;
	data30:		in	std_logic;
	data31:		in	std_logic;
	address:	in	std_logic_vector(4 downto 0);
	output:		out	std_logic
);
end;

-- tested 2015/12/24 as part of testing encoder32xN.  Works.

architecture struct_encoder32x1 of encoder32x1 is
begin
	with address select
		output <=	data0	when	"00000",
					data1	when	"00001",
					data2	when	"00010",
					data3	when	"00011",
					data4	when	"00100",
					data5	when	"00101",
					data6	when	"00110",
					data7	when	"00111",
					data8	when	"01000",
					data9	when	"01001",
					data10	when	"01010",
					data11	when	"01011",
					data12	when	"01100",
					data13	when	"01101",
					data14	when	"01110",
					data15	when	"01111",
					data16	when	"10000",
					data17	when	"10001",
					data18	when	"10010",
					data19	when	"10011",
					data20	when	"10100",
					data21	when	"10101",
					data22	when	"10110",
					data23	when	"10111",
					data24	when	"11000",
					data25	when	"11001",
					data26	when	"11010",
					data27	when	"11011",
					data28	when	"11100",
					data29	when	"11101",
					data30	when	"11110",
					data31	when	"11111",
					'0' when others;
end;


library	ieee;
use		ieee.std_logic_1164.all;

entity encoder32xN is
generic(
	N:	positive
);
port(
	data0:		in	std_logic_vector((N-1) downto 0);
	data1:		in	std_logic_vector((N-1) downto 0);
	data2:		in	std_logic_vector((N-1) downto 0);
	data3:		in	std_logic_vector((N-1) downto 0);
	data4:		in	std_logic_vector((N-1) downto 0);
	data5:		in	std_logic_vector((N-1) downto 0);
	data6:		in	std_logic_vector((N-1) downto 0);
	data7:		in	std_logic_vector((N-1) downto 0);
	data8:		in	std_logic_vector((N-1) downto 0);
	data9:		in	std_logic_vector((N-1) downto 0);
	data10:		in	std_logic_vector((N-1) downto 0);
	data11:		in	std_logic_vector((N-1) downto 0);
	data12:		in	std_logic_vector((N-1) downto 0);
	data13:		in	std_logic_vector((N-1) downto 0);
	data14:		in	std_logic_vector((N-1) downto 0);
	data15:		in	std_logic_vector((N-1) downto 0);
	data16:		in	std_logic_vector((N-1) downto 0);
	data17:		in	std_logic_vector((N-1) downto 0);
	data18:		in	std_logic_vector((N-1) downto 0);
	data19:		in	std_logic_vector((N-1) downto 0);
	data20:		in	std_logic_vector((N-1) downto 0);
	data21:		in	std_logic_vector((N-1) downto 0);
	data22:		in	std_logic_vector((N-1) downto 0);
	data23:		in	std_logic_vector((N-1) downto 0);
	data24:		in	std_logic_vector((N-1) downto 0);
	data25:		in	std_logic_vector((N-1) downto 0);
	data26:		in	std_logic_vector((N-1) downto 0);
	data27:		in	std_logic_vector((N-1) downto 0);
	data28:		in	std_logic_vector((N-1) downto 0);
	data29:		in	std_logic_vector((N-1) downto 0);
	data30:		in	std_logic_vector((N-1) downto 0);
	data31:		in	std_logic_vector((N-1) downto 0);
	address:	in	std_logic_vector(4 downto 0);
	output:		out	std_logic_vector((N-1) downto 0)
);
end;

-- tested 2015/12/24 with N = 8.  Works.

architecture struct_encoder32xN of encoder32xN is
component encoder32x1 is
port(
	data0:		in	std_logic;
	data1:		in	std_logic;
	data2:		in	std_logic;
	data3:		in	std_logic;
	data4:		in	std_logic;
	data5:		in	std_logic;
	data6:		in	std_logic;
	data7:		in	std_logic;
	data8:		in	std_logic;
	data9:		in	std_logic;
	data10:		in	std_logic;
	data11:		in	std_logic;
	data12:		in	std_logic;
	data13:		in	std_logic;
	data14:		in	std_logic;
	data15:		in	std_logic;
	data16:		in	std_logic;
	data17:		in	std_logic;
	data18:		in	std_logic;
	data19:		in	std_logic;
	data20:		in	std_logic;
	data21:		in	std_logic;
	data22:		in	std_logic;
	data23:		in	std_logic;
	data24:		in	std_logic;
	data25:		in	std_logic;
	data26:		in	std_logic;
	data27:		in	std_logic;
	data28:		in	std_logic;
	data29:		in	std_logic;
	data30:		in	std_logic;
	data31:		in	std_logic;
	address:	in	std_logic_vector(4 downto 0);
	output:		out	std_logic
);
end component;

begin
	u1: for i in (N-1) downto 0 generate
		u: encoder32x1 port map(
			data0 => data0(i),
			data1 => data1(i),
			data2 => data2(i),
			data3 => data3(i),
			data4 => data4(i),
			data5 => data5(i),
			data6 => data6(i),
			data7 => data7(i),
			data8 => data8(i),
			data9 => data9(i),
			data10 => data10(i),
			data11 => data11(i),
			data12 => data12(i),
			data13 => data13(i),
			data14 => data14(i),
			data15 => data15(i),
			data16 => data16(i),
			data17 => data17(i),
			data18 => data18(i),
			data19 => data19(i),
			data20 => data20(i),
			data21 => data21(i),
			data22 => data22(i),
			data23 => data23(i),
			data24 => data24(i),
			data25 => data25(i),
			data26 => data26(i),
			data27 => data27(i),
			data28 => data28(i),
			data29 => data29(i),
			data30 => data30(i),
			data31 => data31(i),
			address => address,
			output => output(i)
		);
	end generate u1;
end;

library	ieee;
use		ieee.std_logic_1164.all;

entity tristate is
port(
	a: 		in	std_logic;
	enable:	in	std_logic;
	y: 		out	std_logic
);
end;

-- tested 2015/12/27 with modelsim as part of tristateN testing.  works.
-- tested 2016/01/23 with ghdl as part of tristateN testing.  works.

architecture struct_tristate of tristate is
begin
	y <= a when enable = '1' else 'Z';
end;

library	ieee;
use		ieee.std_logic_1164.all;

entity tristateN is
generic(
	N: positive
);
port(
	a: 		in	std_logic_vector((N-1) downto 0);
	enable:	in	std_logic;
	y: 		out	std_logic_vector((N-1) downto 0)
);
end;

-- tested 2015/12/27 at N = 16 with modelsim.  works.
-- tested 2016/01/23 at N = 16 with ghdl.  works.

architecture struct_tristateN of tristateN is
component tristate is
port(
	a: 		in	std_logic;
	enable:	in	std_logic;
	y: 		out	std_logic
);
end component;

begin
	u1: for i in 0 to (N-1) generate
		u: tristate port map(
			a => a(i),
			enable => enable,
			y => y(i)
		);
	end generate;
end;

library	ieee;
use		ieee.std_logic_1164.all;

entity toggle_ff is
port(
	clk:			in	std_logic;
	clock_enable:	in	std_logic;
	resetn:			in	std_logic;
	q:				out	std_logic
);
end;

-- tested 2015/12/26 with modelsim, works.

architecture struct_toggle_ff of toggle_ff is
component synchronous_latch is
port(
	rstn:			in	std_logic;
	clock:			in	std_logic;
	clock_enable:	in	std_logic;
	d:				in	std_logic;
	q:				out	std_logic
);
end component;

component synchronous_latch_autoclear is
port(
	rstn:			in	std_logic;
	clock:			in	std_logic;
	clock_enable:	in	std_logic;
	d:				in	std_logic;
	q:				out	std_logic
);
end component;

component encoder2x1 is
port(
	data0:		in	std_logic;
	data1:		in	std_logic;
	selector:	in	std_logic;
	output:		out	std_logic
);
end component;

signal	toggle_output:		std_logic;
signal	toggle_clock:		std_logic;
signal	flipflop_data:		std_logic;
signal	not_toggle_output:	std_logic;
signal	notclock:			std_logic;

begin
	u1: synchronous_latch port map(
			rstn => resetn,
			clock => clk,
			clock_enable => toggle_clock,
			d => flipflop_data,
			q => toggle_output
		);

	u2: encoder2x1 port map(
			data0 => toggle_output,
			data1 => not_toggle_output,
			selector => toggle_clock,
			output => flipflop_data
		);

	u3: synchronous_latch_autoclear port map(
			rstn => resetn,
			clock => notclock,
			clock_enable => clock_enable,
			d => clock_enable,
			q => toggle_clock
		);

	not_toggle_output <= not toggle_output;
	notclock <= not clk;
	q <= toggle_output;
end;

library	ieee;
use	ieee.std_logic_1164.all;

entity magnitude is
port(
	a, b:	in	std_logic;
	equal:	out	std_logic;
	lt:		out	std_logic; 	-- '1' if a < b
	gt:		out	std_logic	-- '1' if a > b
);
end;

-- tested 2015/12/26 with modelsim, works.

architecture struct_magnitude of magnitude is

signal	equals:	std_logic;
signal	less:	std_logic;

begin
	equals <= not (a xor b);
	less <= (not a) and b;
	gt <= equals nor less;
	equal <= equals;
	lt <= less;
end;

library	ieee;
use	ieee.std_logic_1164.all;

entity magnitudeN is
generic(
	N:	positive
);
port(
	a, b:	in	std_logic_vector((N-1) downto 0);
	equal:	out	std_logic;
	lt:		out	std_logic; 	-- '1' if a < b
	gt:		out	std_logic	-- '1' if a > b
);
end;

--tested with ghdl 2016/01/26, works.

architecture struct_magnitudeN of magnitudeN is

signal	equals:	std_logic_vector((N-1) downto 0);
signal	less:	std_logic_vector((N-1) downto 0);

begin
	equals(N-1) <= not (a(N-1) xor b(N-1));
	less (N-1) <= (not a(N-1)) and b(N-1);

	u1:	for i in (N-1) downto 1 generate
		equals(i-1) <= equals(i) and (not (a(i-1) xor b(i-1)));
		less(i-1) <= less(i) or (((not a(i-1)) and b(i-1)) and equals(i));
	end generate u1;

	equal <= equals(0);
	lt <= less(0);
	gt <= equals(0) nor less(0);
end;


--library	ieee;
--use	ieee.std_logic_1164.all;
--
--entity magnitude2 is
--port(
--	a, b:	in	std_logic_vector(1 downto 0);
--	equal:	out	std_logic;
--	lt:		out	std_logic; 	-- '1' if a < b
--	gt:		out	std_logic	-- '1' if a > b
--);
--end;
--
---- tested 2015/12/26 with modelsim, works.
--
--architecture struct_magnitude2 of magnitude2 is
--component magnitude is
--port(
--	a, b:	in	std_logic;
--	equal:	out	std_logic;
--	lt:		out	std_logic; 	-- '1' if a < b
--	gt:		out	std_logic	-- '1' if a > b
--);
--end component;
--
--signal	high_equals:	std_logic;
--signal	high_lt:	   	std_logic;
--signal	high_gt:		std_logic;
--
--signal	low_equals:		std_logic;
--signal	low_lt:	   		std_logic;
--signal	low_gt:			std_logic;
--
--signal	equals:			std_logic;
--signal	less:			std_logic;
--
--begin
--	u1: magnitude port map(
--		a => a(1),
--		b => b(1),
--		equal => high_equals,
--		lt => high_lt,
--		gt => high_gt
--	);
--
--	u2: magnitude port map(
--		a => a(0),
--		b => b(0),
--		equal => low_equals,
--		lt => low_lt,
--		gt => low_gt
--	);
--
--	equals <= high_equals and low_equals;
--	less <= high_lt or (high_equals and low_lt);
--	gt <= equals nor less;
--	equal <= equals;
--	lt <= less;
--end;
--
--library	ieee;
--use	ieee.std_logic_1164.all;
--
--entity magnitude3 is
--port(
--	a, b:	in	std_logic_vector(2 downto 0);
--	equal:	out	std_logic;
--	lt:		out	std_logic; 	-- '1' if a < b
--	gt:		out	std_logic	-- '1' if a > b
--);
--end;
--
---- tested 2015/12/26 with modelsim, works.
--
--architecture struct_magnitude3 of magnitude3 is
--component magnitude2 is
--port(
--	a, b:	in	std_logic_vector(1 downto 0);
--	equal:	out	std_logic;
--	lt:		out	std_logic; 	-- '1' if a < b
--	gt:		out	std_logic	-- '1' if a > b
--);
--end component;
--
--component magnitude is
--port(
--	a, b:	in	std_logic;
--	equal:	out	std_logic;
--	lt:		out	std_logic; 	-- '1' if a < b
--	gt:		out	std_logic	-- '1' if a > b
--);
--end component;
--
--signal	high_equals:	std_logic;
--signal	high_lt:	   	std_logic;
--signal	high_gt:		std_logic;
--
--signal	low_equals:		std_logic;
--signal	low_lt:	   		std_logic;
--signal	low_gt:			std_logic;
--
--signal	equals:			std_logic;
--signal	less:			std_logic;
--
--begin
--	u1: magnitude port map(
--		a => a(2),
--		b => b(2),
--		equal => high_equals,
--		lt => high_lt,
--		gt => high_gt
--	);
--
--	u2: magnitude2 port map(
--		a => a(1 downto 0),
--		b => b(1 downto 0),
--		equal => low_equals,
--		lt => low_lt,
--		gt => low_gt
--	);
--
--	equals <= high_equals and low_equals;
--	less <= high_lt or (high_equals and low_lt);
--	gt <= equals nor less;
--	equal <= equals;
--	lt <= less;
--end;
--
--library	ieee;
--use	ieee.std_logic_1164.all;
--
--entity magnitude4 is
--port(
--	a, b:	in	std_logic_vector(3 downto 0);
--	equal:	out	std_logic;
--	lt:		out	std_logic; 	-- '1' if a < b
--	gt:		out	std_logic	-- '1' if a > b
--);
--end;
--
---- tested 2015/12/26 with modelsim, works.
--
--architecture struct_magnitude4 of magnitude4 is
--component magnitude3 is
--port(
--	a, b:	in	std_logic_vector(2 downto 0);
--	equal:	out	std_logic;
--	lt:		out	std_logic; 	-- '1' if a < b
--	gt:		out	std_logic	-- '1' if a > b
--);
--end component;
--
--component magnitude is
--port(
--	a, b:	in	std_logic;
--	equal:	out	std_logic;
--	lt:		out	std_logic; 	-- '1' if a < b
--	gt:		out	std_logic	-- '1' if a > b
--);
--end component;
--
--signal	high_equals:	std_logic;
--signal	high_lt:	   	std_logic;
--signal	high_gt:		std_logic;
--
--signal	low_equals:		std_logic;
--signal	low_lt:	   		std_logic;
--signal	low_gt:			std_logic;
--
--signal	equals:			std_logic;
--signal	less:			std_logic;
--
--begin
--	u1: magnitude port map(
--		a => a(3),
--		b => b(3),
--		equal => high_equals,
--		lt => high_lt,
--		gt => high_gt
--	);
--
--	u2: magnitude3 port map(
--		a => a(2 downto 0),
--		b => b(2 downto 0),
--		equal => low_equals,
--		lt => low_lt,
--		gt => low_gt
--	);
--
--	equals <= high_equals and low_equals;
--	less <= high_lt or (high_equals and low_lt);
--	gt <= equals nor less;
--	equal <= equals;
--	lt <= less;
--end;
--
--library	ieee;
--use	ieee.std_logic_1164.all;
--
--entity magnitude5 is
--port(
--	a, b:	in	std_logic_vector(4 downto 0);
--	equal:	out	std_logic;
--	lt:		out	std_logic; 	-- '1' if a < b
--	gt:		out	std_logic	-- '1' if a > b
--);
--end;
--
---- tested 2015/12/26 with modelsim, works.
--
--architecture struct_magnitude5 of magnitude5 is
--component magnitude4 is
--port(
--	a, b:	in	std_logic_vector(3 downto 0);
--	equal:	out	std_logic;
--	lt:		out	std_logic; 	-- '1' if a < b
--	gt:		out	std_logic	-- '1' if a > b
--);
--end component;
--
--component magnitude is
--port(
--	a, b:	in	std_logic;
--	equal:	out	std_logic;
--	lt:		out	std_logic; 	-- '1' if a < b
--	gt:		out	std_logic	-- '1' if a > b
--);
--end component;
--
--signal	high_equals:	std_logic;
--signal	high_lt:	   	std_logic;
--signal	high_gt:		std_logic;
--
--signal	low_equals:		std_logic;
--signal	low_lt:	   		std_logic;
--signal	low_gt:			std_logic;
--
--signal	equals:			std_logic;
--signal	less:			std_logic;
--
--begin
--	u1: magnitude port map(
--		a => a(4),
--		b => b(4),
--		equal => high_equals,
--		lt => high_lt,
--		gt => high_gt
--	);
--
--	u2: magnitude4 port map(
--		a => a(3 downto 0),
--		b => b(3 downto 0),
--		equal => low_equals,
--		lt => low_lt,
--		gt => low_gt
--	);
--
--	equals <= high_equals and low_equals;
--	less <= high_lt or (high_equals and low_lt);
--	gt <= equals nor less;
--	equal <= equals;
--	lt <= less;
--end;
--
--library	ieee;
--use	ieee.std_logic_1164.all;
--
--entity magnitude6 is
--port(
--	a, b:	in	std_logic_vector(5 downto 0);
--	equal:	out	std_logic;
--	lt:		out	std_logic; 	-- '1' if a < b
--	gt:		out	std_logic	-- '1' if a > b
--);
--end;
--
---- tested 2015/12/26 with modelsim, works.
--
--architecture struct_magnitude6 of magnitude6 is
--component magnitude5 is
--port(
--	a, b:	in	std_logic_vector(4 downto 0);
--	equal:	out	std_logic;
--	lt:		out	std_logic; 	-- '1' if a < b
--	gt:		out	std_logic	-- '1' if a > b
--);
--end component;
--
--component magnitude is
--port(
--	a, b:	in	std_logic;
--	equal:	out	std_logic;
--	lt:		out	std_logic; 	-- '1' if a < b
--	gt:		out	std_logic	-- '1' if a > b
--);
--end component;
--
--signal	high_equals:	std_logic;
--signal	high_lt:	   	std_logic;
--signal	high_gt:		std_logic;
--
--signal	low_equals:		std_logic;
--signal	low_lt:	   		std_logic;
--signal	low_gt:			std_logic;
--
--signal	equals:			std_logic;
--signal	less:			std_logic;
--
--begin
--	u1: magnitude port map(
--		a => a(5),
--		b => b(5),
--		equal => high_equals,
--		lt => high_lt,
--		gt => high_gt
--	);
--
--	u2: magnitude5 port map(
--		a => a(4 downto 0),
--		b => b(4 downto 0),
--		equal => low_equals,
--		lt => low_lt,
--		gt => low_gt
--	);
--
--	equals <= high_equals and low_equals;
--	less <= high_lt or (high_equals and low_lt);
--	gt <= equals nor less;
--	equal <= equals;
--	lt <= less;
--end;
--
--library	ieee;
--use	ieee.std_logic_1164.all;
--
--entity magnitude7 is
--port(
--	a, b:	in	std_logic_vector(6 downto 0);
--	equal:	out	std_logic;
--	lt:		out	std_logic; 	-- '1' if a < b
--	gt:		out	std_logic	-- '1' if a > b
--);
--end;
--
---- tested 2015/12/26 with modelsim, works.
--
--architecture struct_magnitude7 of magnitude7 is
--component magnitude6 is
--port(
--	a, b:	in	std_logic_vector(5 downto 0);
--	equal:	out	std_logic;
--	lt:		out	std_logic; 	-- '1' if a < b
--	gt:		out	std_logic	-- '1' if a > b
--);
--end component;
--
--component magnitude is
--port(
--	a, b:	in	std_logic;
--	equal:	out	std_logic;
--	lt:		out	std_logic; 	-- '1' if a < b
--	gt:		out	std_logic	-- '1' if a > b
--);
--end component;
--
--signal	high_equals:	std_logic;
--signal	high_lt:	   	std_logic;
--signal	high_gt:		std_logic;
--
--signal	low_equals:		std_logic;
--signal	low_lt:	   		std_logic;
--signal	low_gt:			std_logic;
--
--signal	equals:			std_logic;
--signal	less:			std_logic;
--
--begin
--	u1: magnitude port map(
--		a => a(6),
--		b => b(6),
--		equal => high_equals,
--		lt => high_lt,
--		gt => high_gt
--	);
--
--	u2: magnitude6 port map(
--		a => a(5 downto 0),
--		b => b(5 downto 0),
--		equal => low_equals,
--		lt => low_lt,
--		gt => low_gt
--	);
--
--	equals <= high_equals and low_equals;
--	less <= high_lt or (high_equals and low_lt);
--	gt <= equals nor less;
--	equal <= equals;
--	lt <= less;
--end;
--
--library	ieee;
--use	ieee.std_logic_1164.all;
--
--entity magnitude8 is
--port(
--	a, b:	in	std_logic_vector(7 downto 0);
--	equal:	out	std_logic;
--	lt:		out	std_logic; 	-- '1' if a < b
--	gt:		out	std_logic	-- '1' if a > b
--);
--end;
--
---- tested 2015/12/26 with modelsim, works.
--architecture struct_magnitude8 of magnitude8 is
--component magnitude7 is
--port(
--	a, b:	in	std_logic_vector(6 downto 0);
--	equal:	out	std_logic;
--	lt:		out	std_logic; 	-- '1' if a < b
--	gt:		out	std_logic	-- '1' if a > b
--);
--end component;
--
--component magnitude is
--port(
--	a, b:	in	std_logic;
--	equal:	out	std_logic;
--	lt:		out	std_logic; 	-- '1' if a < b
--	gt:		out	std_logic	-- '1' if a > b
--);
--end component;
--
--signal	high_equals:	std_logic;
--signal	high_lt:	   	std_logic;
--signal	high_gt:		std_logic;
--
--signal	low_equals:		std_logic;
--signal	low_lt:	   		std_logic;
--signal	low_gt:			std_logic;
--
--signal	equals:			std_logic;
--signal	less:			std_logic;
--
--begin
--	u1: magnitude port map(
--		a => a(7),
--		b => b(7),
--		equal => high_equals,
--		lt => high_lt,
--		gt => high_gt
--	);
--
--	u2: magnitude7 port map(
--		a => a(6 downto 0),
--		b => b(6 downto 0),
--		equal => low_equals,
--		lt => low_lt,
--		gt => low_gt
--	);
--
--	equals <= high_equals and low_equals;
--	less <= high_lt or (high_equals and low_lt);
--	gt <= equals nor less;
--	equal <= equals;
--	lt <= less;
--end;

library	ieee;
use	ieee.std_logic_1164.all;

entity encoder64x1 is
port(
	data0:		in	std_logic;
	data1:		in	std_logic;
	data2:		in	std_logic;
	data3:		in	std_logic;
	data4:		in	std_logic;
	data5:		in	std_logic;
	data6:		in	std_logic;
	data7:		in	std_logic;
	data8:		in	std_logic;
	data9:		in	std_logic;
	data10:		in	std_logic;
	data11:		in	std_logic;
	data12:		in	std_logic;
	data13:		in	std_logic;
	data14:		in	std_logic;
	data15:		in	std_logic;
	data16:		in	std_logic;
	data17:		in	std_logic;
	data18:		in	std_logic;
	data19:		in	std_logic;
	data20:		in	std_logic;
	data21:		in	std_logic;
	data22:		in	std_logic;
	data23:		in	std_logic;
	data24:		in	std_logic;
	data25:		in	std_logic;
	data26:		in	std_logic;
	data27:		in	std_logic;
	data28:		in	std_logic;
	data29:		in	std_logic;
	data30:		in	std_logic;
	data31:		in	std_logic;
	data32:		in	std_logic;
	data33:		in	std_logic;
	data34:		in	std_logic;
	data35:		in	std_logic;
	data36:		in	std_logic;
	data37:		in	std_logic;
	data38:		in	std_logic;
	data39:		in	std_logic;
	data40:		in	std_logic;
	data41:		in	std_logic;
	data42:		in	std_logic;
	data43:		in	std_logic;
	data44:		in	std_logic;
	data45:		in	std_logic;
	data46:		in	std_logic;
	data47:		in	std_logic;
	data48:		in	std_logic;
	data49:		in	std_logic;
	data50:		in	std_logic;
	data51:		in	std_logic;
	data52:		in	std_logic;
	data53:		in	std_logic;
	data54:		in	std_logic;
	data55:		in	std_logic;
	data56:		in	std_logic;
	data57:		in	std_logic;
	data58:		in	std_logic;
	data59:		in	std_logic;
	data60:		in	std_logic;
	data61:		in	std_logic;
	data62:		in	std_logic;
	data63:		in	std_logic;
	address:	in	std_logic_vector(5 downto 0);
	output:		out	std_logic
);
end;

-- tested 2015/12/24 with modelsim as part of encoder64xN. works.

architecture struct_encoder64x1 of encoder64x1 is
begin
	with address select
		output <=
			data0	when "000000",
			data1	when "000001",
			data2	when "000010",
			data3	when "000011",
			data4	when "000100",
			data5	when "000101",
			data6	when "000110",
			data7	when "000111",
			data8	when "001000",
			data9	when "001001",
			data10	when "001010",
			data11	when "001011",
			data12	when "001100",
			data13	when "001101",
			data14	when "001110",
			data15	when "001111",
			data16	when "010000",
			data17	when "010001",
			data18	when "010010",
			data19	when "010011",
			data20	when "010100",
			data21	when "010101",
			data22	when "010110",
			data23	when "010111",
			data24	when "011000",
			data25	when "011001",
			data26	when "011010",
			data27	when "011011",
			data28	when "011100",
			data29	when "011101",
			data30	when "011110",
			data31	when "011111",
			data32	when "100000",
			data33	when "100001",
			data34	when "100010",
			data35	when "100011",
			data36	when "100100",
			data37	when "100101",
			data38	when "100110",
			data39	when "100111",
			data40	when "101000",
			data41	when "101001",
			data42	when "101010",
			data43	when "101011",
			data44	when "101100",
			data45	when "101101",
			data46	when "101110",
			data47	when "101111",
			data48	when "110000",
			data49	when "110001",
			data50	when "110010",
			data51	when "110011",
			data52	when "110100",
			data53	when "110101",
			data54	when "110110",
			data55	when "110111",
			data56	when "111000",
			data57	when "111001",
			data58	when "111010",
			data59	when "111011",
			data60	when "111100",
			data61	when "111101",
			data62	when "111110",
			data63	when "111111",
			'0' when others;
end;


library	ieee;
use	ieee.std_logic_1164.all;

entity encoder64xN is
generic(
	N:	positive
);
port(
	data0:		in	std_logic_vector((N-1) downto 0);
	data1:		in	std_logic_vector((N-1) downto 0);
	data2:		in	std_logic_vector((N-1) downto 0);
	data3:		in	std_logic_vector((N-1) downto 0);
	data4:		in	std_logic_vector((N-1) downto 0);
	data5:		in	std_logic_vector((N-1) downto 0);
	data6:		in	std_logic_vector((N-1) downto 0);
	data7:		in	std_logic_vector((N-1) downto 0);
	data8:		in	std_logic_vector((N-1) downto 0);
	data9:		in	std_logic_vector((N-1) downto 0);
	data10:		in	std_logic_vector((N-1) downto 0);
	data11:		in	std_logic_vector((N-1) downto 0);
	data12:		in	std_logic_vector((N-1) downto 0);
	data13:		in	std_logic_vector((N-1) downto 0);
	data14:		in	std_logic_vector((N-1) downto 0);
	data15:		in	std_logic_vector((N-1) downto 0);
	data16:		in	std_logic_vector((N-1) downto 0);
	data17:		in	std_logic_vector((N-1) downto 0);
	data18:		in	std_logic_vector((N-1) downto 0);
	data19:		in	std_logic_vector((N-1) downto 0);
	data20:		in	std_logic_vector((N-1) downto 0);
	data21:		in	std_logic_vector((N-1) downto 0);
	data22:		in	std_logic_vector((N-1) downto 0);
	data23:		in	std_logic_vector((N-1) downto 0);
	data24:		in	std_logic_vector((N-1) downto 0);
	data25:		in	std_logic_vector((N-1) downto 0);
	data26:		in	std_logic_vector((N-1) downto 0);
	data27:		in	std_logic_vector((N-1) downto 0);
	data28:		in	std_logic_vector((N-1) downto 0);
	data29:		in	std_logic_vector((N-1) downto 0);
	data30:		in	std_logic_vector((N-1) downto 0);
	data31:		in	std_logic_vector((N-1) downto 0);
	data32:		in	std_logic_vector((N-1) downto 0);
	data33:		in	std_logic_vector((N-1) downto 0);
	data34:		in	std_logic_vector((N-1) downto 0);
	data35:		in	std_logic_vector((N-1) downto 0);
	data36:		in	std_logic_vector((N-1) downto 0);
	data37:		in	std_logic_vector((N-1) downto 0);
	data38:		in	std_logic_vector((N-1) downto 0);
	data39:		in	std_logic_vector((N-1) downto 0);
	data40:		in	std_logic_vector((N-1) downto 0);
	data41:		in	std_logic_vector((N-1) downto 0);
	data42:		in	std_logic_vector((N-1) downto 0);
	data43:		in	std_logic_vector((N-1) downto 0);
	data44:		in	std_logic_vector((N-1) downto 0);
	data45:		in	std_logic_vector((N-1) downto 0);
	data46:		in	std_logic_vector((N-1) downto 0);
	data47:		in	std_logic_vector((N-1) downto 0);
	data48:		in	std_logic_vector((N-1) downto 0);
	data49:		in	std_logic_vector((N-1) downto 0);
	data50:		in	std_logic_vector((N-1) downto 0);
	data51:		in	std_logic_vector((N-1) downto 0);
	data52:		in	std_logic_vector((N-1) downto 0);
	data53:		in	std_logic_vector((N-1) downto 0);
	data54:		in	std_logic_vector((N-1) downto 0);
	data55:		in	std_logic_vector((N-1) downto 0);
	data56:		in	std_logic_vector((N-1) downto 0);
	data57:		in	std_logic_vector((N-1) downto 0);
	data58:		in	std_logic_vector((N-1) downto 0);
	data59:		in	std_logic_vector((N-1) downto 0);
	data60:		in	std_logic_vector((N-1) downto 0);
	data61:		in	std_logic_vector((N-1) downto 0);
	data62:		in	std_logic_vector((N-1) downto 0);
	data63:		in	std_logic_vector((N-1) downto 0);
	address:	in	std_logic_vector(5 downto 0);
	output:		out	std_logic_vector((N-1) downto 0)
);
end;

-- tested 2015/12/24 with modelsim and N = 8. works.

architecture struct_encoder64xN of encoder64xN is
component encoder64x1 is
port(
	data0:		in	std_logic;
	data1:		in	std_logic;
	data2:		in	std_logic;
	data3:		in	std_logic;
	data4:		in	std_logic;
	data5:		in	std_logic;
	data6:		in	std_logic;
	data7:		in	std_logic;
	data8:		in	std_logic;
	data9:		in	std_logic;
	data10:		in	std_logic;
	data11:		in	std_logic;
	data12:		in	std_logic;
	data13:		in	std_logic;
	data14:		in	std_logic;
	data15:		in	std_logic;
	data16:		in	std_logic;
	data17:		in	std_logic;
	data18:		in	std_logic;
	data19:		in	std_logic;
	data20:		in	std_logic;
	data21:		in	std_logic;
	data22:		in	std_logic;
	data23:		in	std_logic;
	data24:		in	std_logic;
	data25:		in	std_logic;
	data26:		in	std_logic;
	data27:		in	std_logic;
	data28:		in	std_logic;
	data29:		in	std_logic;
	data30:		in	std_logic;
	data31:		in	std_logic;
	data32:		in	std_logic;
	data33:		in	std_logic;
	data34:		in	std_logic;
	data35:		in	std_logic;
	data36:		in	std_logic;
	data37:		in	std_logic;
	data38:		in	std_logic;
	data39:		in	std_logic;
	data40:		in	std_logic;
	data41:		in	std_logic;
	data42:		in	std_logic;
	data43:		in	std_logic;
	data44:		in	std_logic;
	data45:		in	std_logic;
	data46:		in	std_logic;
	data47:		in	std_logic;
	data48:		in	std_logic;
	data49:		in	std_logic;
	data50:		in	std_logic;
	data51:		in	std_logic;
	data52:		in	std_logic;
	data53:		in	std_logic;
	data54:		in	std_logic;
	data55:		in	std_logic;
	data56:		in	std_logic;
	data57:		in	std_logic;
	data58:		in	std_logic;
	data59:		in	std_logic;
	data60:		in	std_logic;
	data61:		in	std_logic;
	data62:		in	std_logic;
	data63:		in	std_logic;
	address:	in	std_logic_vector(5 downto 0);
	output:		out	std_logic
);
end component;

begin
	u1:	for i in 0 to (N-1) generate
		u: encoder64x1 port map(
			data0 => data0(i),
			data1 => data1(i),
			data2 => data2(i),
			data3 => data3(i),
			data4 => data4(i),
			data5 => data5(i),
			data6 => data6(i),
			data7 => data7(i),
			data8 => data8(i),
			data9 => data9(i),
			data10 => data10(i),
			data11 => data11(i),
			data12 => data12(i),
			data13 => data13(i),
			data14 => data14(i),
			data15 => data15(i),
			data16 => data16(i),
			data17 => data17(i),
			data18 => data18(i),
			data19 => data19(i),
			data20 => data20(i),
			data21 => data21(i),
			data22 => data22(i),
			data23 => data23(i),
			data24 => data24(i),
			data25 => data25(i),
			data26 => data26(i),
			data27 => data27(i),
			data28 => data28(i),
			data29 => data29(i),
			data30 => data30(i),
			data31 => data31(i),
			data32 => data32(i),
			data33 => data33(i),
			data34 => data34(i),
			data35 => data35(i),
			data36 => data36(i),
			data37 => data37(i),
			data38 => data38(i),
			data39 => data39(i),
			data40 => data40(i),
			data41 => data41(i),
			data42 => data42(i),
			data43 => data43(i),
			data44 => data44(i),
			data45 => data45(i),
			data46 => data46(i),
			data47 => data47(i),
			data48 => data48(i),
			data49 => data49(i),
			data50 => data50(i),
			data51 => data51(i),
			data52 => data52(i),
			data53 => data53(i),
			data54 => data54(i),
			data55 => data55(i),
			data56 => data56(i),
			data57 => data57(i),
			data58 => data58(i),
			data59 => data59(i),
			data60 => data60(i),
			data61 => data61(i),
			data62 => data62(i),
			data63 => data63(i),
			address => address,
			output => output(i)
		);
	end generate u1;
end;

library	ieee;
use	ieee.std_logic_1164.all;

entity decoder1x64 is
port(
	data:		in	std_logic;
	y0:			out	std_logic;
	y1:			out	std_logic;
	y2:			out	std_logic;
	y3:			out	std_logic;
	y4:			out	std_logic;
	y5:			out	std_logic;
	y6:			out	std_logic;
	y7:			out	std_logic;
	y8:			out	std_logic;
	y9:			out	std_logic;
	y10:		out	std_logic;
	y11:		out	std_logic;
	y12:		out	std_logic;
	y13:		out	std_logic;
	y14:		out	std_logic;
	y15:		out	std_logic;
	y16:		out	std_logic;
	y17:		out	std_logic;
	y18:		out	std_logic;
	y19:		out	std_logic;
	y20:		out	std_logic;
	y21:		out	std_logic;
	y22:		out	std_logic;
	y23:		out	std_logic;
	y24:		out	std_logic;
	y25:		out	std_logic;
	y26:		out	std_logic;
	y27:		out	std_logic;
	y28:		out	std_logic;
	y29:		out	std_logic;
	y30:		out	std_logic;
	y31:		out	std_logic;
	y32:		out	std_logic;
	y33:		out	std_logic;
	y34:		out	std_logic;
	y35:		out	std_logic;
	y36:		out	std_logic;
	y37:		out	std_logic;
	y38:		out	std_logic;
	y39:		out	std_logic;
	y40:		out	std_logic;
	y41:		out	std_logic;
	y42:		out	std_logic;
	y43:		out	std_logic;
	y44:		out	std_logic;
	y45:		out	std_logic;
	y46:		out	std_logic;
	y47:		out	std_logic;
	y48:		out	std_logic;
	y49:		out	std_logic;
	y50:		out	std_logic;
	y51:		out	std_logic;
	y52:		out	std_logic;
	y53:		out	std_logic;
	y54:		out	std_logic;
	y55:		out	std_logic;
	y56:		out	std_logic;
	y57:		out	std_logic;
	y58:		out	std_logic;
	y59:		out	std_logic;
	y60:		out	std_logic;
	y61:		out	std_logic;
	y62:		out	std_logic;
	y63:		out	std_logic;
	address:	in	std_logic_vector(5 downto 0)
);
end;

-- tested 2015/12/26 as part of decoderNx64 using modelsim. works.

architecture struct_decoder1x64 of decoder1x64 is
begin
	with address select y0 	<= data when "000000", '0' when others;
	with address select y1	<= data when "000001", '0' when others;
	with address select y2 	<= data when "000010", '0' when others;
	with address select	y3	<= data when "000011", '0' when others;
	with address select	y4	<= data when "000100", '0' when others;
	with address select	y5	<= data when "000101", '0' when others;
	with address select y6	<= data when "000110", '0' when others;
	with address select y7	<= data when "000111", '0' when others;
	with address select y8 	<= data when "001000", '0' when others;
	with address select y9	<= data when "001001", '0' when others;
	with address select y10	<= data when "001010", '0' when others;
	with address select	y11	<= data when "001011", '0' when others;
	with address select	y12	<= data when "001100", '0' when others;
	with address select	y13	<= data when "001101", '0' when others;
	with address select y14	<= data when "001110", '0' when others;
	with address select y15	<= data when "001111", '0' when others;
	with address select y16	<= data when "010000", '0' when others;
	with address select y17	<= data when "010001", '0' when others;
	with address select y18	<= data when "010010", '0' when others;
	with address select	y19	<= data when "010011", '0' when others;
	with address select	y20	<= data when "010100", '0' when others;
	with address select	y21	<= data when "010101", '0' when others;
	with address select y22	<= data when "010110", '0' when others;
	with address select y23	<= data when "010111", '0' when others;
	with address select y24	<= data when "011000", '0' when others;
	with address select y25	<= data when "011001", '0' when others;
	with address select y26	<= data when "011010", '0' when others;
	with address select	y27	<= data when "011011", '0' when others;
	with address select	y28	<= data when "011100", '0' when others;
	with address select	y29	<= data when "011101", '0' when others;
	with address select y30	<= data when "011110", '0' when others;
	with address select y31	<= data when "011111", '0' when others;
	with address select y32	<= data when "100000", '0' when others;
	with address select y33	<= data when "100001", '0' when others;
	with address select y34	<= data when "100010", '0' when others;
	with address select	y35	<= data when "100011", '0' when others;
	with address select	y36	<= data when "100100", '0' when others;
	with address select	y37	<= data when "100101", '0' when others;
	with address select y38	<= data when "100110", '0' when others;
	with address select y39	<= data when "100111", '0' when others;
	with address select y40	<= data when "101000", '0' when others;
	with address select y41	<= data when "101001", '0' when others;
	with address select y42	<= data when "101010", '0' when others;
	with address select	y43	<= data when "101011", '0' when others;
	with address select	y44	<= data when "101100", '0' when others;
	with address select	y45	<= data when "101101", '0' when others;
	with address select y46	<= data when "101110", '0' when others;
	with address select y47	<= data when "101111", '0' when others;
	with address select y48	<= data when "110000", '0' when others;
	with address select y49	<= data when "110001", '0' when others;
	with address select y50	<= data when "110010", '0' when others;
	with address select	y51	<= data when "110011", '0' when others;
	with address select	y52	<= data when "110100", '0' when others;
	with address select	y53	<= data when "110101", '0' when others;
	with address select y54	<= data when "110110", '0' when others;
	with address select y55	<= data when "110111", '0' when others;
	with address select y56	<= data when "111000", '0' when others;
	with address select y57	<= data when "111001", '0' when others;
	with address select y58	<= data when "111010", '0' when others;
	with address select	y59	<= data when "111011", '0' when others;
	with address select	y60	<= data when "111100", '0' when others;
	with address select	y61	<= data when "111101", '0' when others;
	with address select y62	<= data when "111110", '0' when others;
	with address select y63	<= data when "111111", '0' when others;
end;

library	ieee;
use	ieee.std_logic_1164.all;

entity decoderNx64 is
generic(
	N:	positive
);
port(
	data:		in	std_logic_vector((N-1) downto 0);
	y0:			out	std_logic_vector((N-1) downto 0);
	y1:			out	std_logic_vector((N-1) downto 0);
	y2:			out	std_logic_vector((N-1) downto 0);
	y3:			out	std_logic_vector((N-1) downto 0);
	y4:			out	std_logic_vector((N-1) downto 0);
	y5:			out	std_logic_vector((N-1) downto 0);
	y6:			out	std_logic_vector((N-1) downto 0);
	y7:			out	std_logic_vector((N-1) downto 0);
	y8:			out	std_logic_vector((N-1) downto 0);
	y9:			out	std_logic_vector((N-1) downto 0);
	y10:		out	std_logic_vector((N-1) downto 0);
	y11:		out	std_logic_vector((N-1) downto 0);
	y12:		out	std_logic_vector((N-1) downto 0);
	y13:		out	std_logic_vector((N-1) downto 0);
	y14:		out	std_logic_vector((N-1) downto 0);
	y15:		out	std_logic_vector((N-1) downto 0);
	y16:		out	std_logic_vector((N-1) downto 0);
	y17:		out	std_logic_vector((N-1) downto 0);
	y18:		out	std_logic_vector((N-1) downto 0);
	y19:		out	std_logic_vector((N-1) downto 0);
	y20:		out	std_logic_vector((N-1) downto 0);
	y21:		out	std_logic_vector((N-1) downto 0);
	y22:		out	std_logic_vector((N-1) downto 0);
	y23:		out	std_logic_vector((N-1) downto 0);
	y24:		out	std_logic_vector((N-1) downto 0);
	y25:		out	std_logic_vector((N-1) downto 0);
	y26:		out	std_logic_vector((N-1) downto 0);
	y27:		out	std_logic_vector((N-1) downto 0);
	y28:		out	std_logic_vector((N-1) downto 0);
	y29:		out	std_logic_vector((N-1) downto 0);
	y30:		out	std_logic_vector((N-1) downto 0);
	y31:		out	std_logic_vector((N-1) downto 0);
	y32:		out	std_logic_vector((N-1) downto 0);
	y33:		out	std_logic_vector((N-1) downto 0);
	y34:		out	std_logic_vector((N-1) downto 0);
	y35:		out	std_logic_vector((N-1) downto 0);
	y36:		out	std_logic_vector((N-1) downto 0);
	y37:		out	std_logic_vector((N-1) downto 0);
	y38:		out	std_logic_vector((N-1) downto 0);
	y39:		out	std_logic_vector((N-1) downto 0);
	y40:		out	std_logic_vector((N-1) downto 0);
	y41:		out	std_logic_vector((N-1) downto 0);
	y42:		out	std_logic_vector((N-1) downto 0);
	y43:		out	std_logic_vector((N-1) downto 0);
	y44:		out	std_logic_vector((N-1) downto 0);
	y45:		out	std_logic_vector((N-1) downto 0);
	y46:		out	std_logic_vector((N-1) downto 0);
	y47:		out	std_logic_vector((N-1) downto 0);
	y48:		out	std_logic_vector((N-1) downto 0);
	y49:		out	std_logic_vector((N-1) downto 0);
	y50:		out	std_logic_vector((N-1) downto 0);
	y51:		out	std_logic_vector((N-1) downto 0);
	y52:		out	std_logic_vector((N-1) downto 0);
	y53:		out	std_logic_vector((N-1) downto 0);
	y54:		out	std_logic_vector((N-1) downto 0);
	y55:		out	std_logic_vector((N-1) downto 0);
	y56:		out	std_logic_vector((N-1) downto 0);
	y57:		out	std_logic_vector((N-1) downto 0);
	y58:		out	std_logic_vector((N-1) downto 0);
	y59:		out	std_logic_vector((N-1) downto 0);
	y60:		out	std_logic_vector((N-1) downto 0);
	y61:		out	std_logic_vector((N-1) downto 0);
	y62:		out	std_logic_vector((N-1) downto 0);
	y63:		out	std_logic_vector((N-1) downto 0);
	address:	in	std_logic_vector(5 downto 0)
);
end;

-- tested 2015/12/26 with N = 8 using modelsim. works.

architecture struct_decoderNx64 of decoderNx64 is
component decoder1x64 is
port(
	data:		in	std_logic;
	y0:			out	std_logic;
	y1:			out	std_logic;
	y2:			out	std_logic;
	y3:			out	std_logic;
	y4:			out	std_logic;
	y5:			out	std_logic;
	y6:			out	std_logic;
	y7:			out	std_logic;
	y8:			out	std_logic;
	y9:			out	std_logic;
	y10:		out	std_logic;
	y11:		out	std_logic;
	y12:		out	std_logic;
	y13:		out	std_logic;
	y14:		out	std_logic;
	y15:		out	std_logic;
	y16:		out	std_logic;
	y17:		out	std_logic;
	y18:		out	std_logic;
	y19:		out	std_logic;
	y20:		out	std_logic;
	y21:		out	std_logic;
	y22:		out	std_logic;
	y23:		out	std_logic;
	y24:		out	std_logic;
	y25:		out	std_logic;
	y26:		out	std_logic;
	y27:		out	std_logic;
	y28:		out	std_logic;
	y29:		out	std_logic;
	y30:		out	std_logic;
	y31:		out	std_logic;
	y32:		out	std_logic;
	y33:		out	std_logic;
	y34:		out	std_logic;
	y35:		out	std_logic;
	y36:		out	std_logic;
	y37:		out	std_logic;
	y38:		out	std_logic;
	y39:		out	std_logic;
	y40:		out	std_logic;
	y41:		out	std_logic;
	y42:		out	std_logic;
	y43:		out	std_logic;
	y44:		out	std_logic;
	y45:		out	std_logic;
	y46:		out	std_logic;
	y47:		out	std_logic;
	y48:		out	std_logic;
	y49:		out	std_logic;
	y50:		out	std_logic;
	y51:		out	std_logic;
	y52:		out	std_logic;
	y53:		out	std_logic;
	y54:		out	std_logic;
	y55:		out	std_logic;
	y56:		out	std_logic;
	y57:		out	std_logic;
	y58:		out	std_logic;
	y59:		out	std_logic;
	y60:		out	std_logic;
	y61:		out	std_logic;
	y62:		out	std_logic;
	y63:		out	std_logic;
	address:	in	std_logic_vector(5 downto 0)
);
end component;

begin
	u1:	for i in 0 to (N-1) generate
		u: decoder1x64 port map(
			data => data(i),
			y0 => y0(i),
			y1 => y1(i),
			y2 => y2(i),
			y3 => y3(i),
			y4 => y4(i),
			y5 => y5(i),
			y6 => y6(i),
			y7 => y7(i),
			y8 => y8(i),
			y9 => y9(i),
			y10 => y10(i),
			y11 => y11(i),
			y12 => y12(i),
			y13 => y13(i),
			y14 => y14(i),
			y15 => y15(i),
			y16 => y16(i),
			y17 => y17(i),
			y18 => y18(i),
			y19 => y19(i),
			y20 => y20(i),
			y21 => y21(i),
			y22 => y22(i),
			y23 => y23(i),
			y24 => y24(i),
			y25 => y25(i),
			y26 => y26(i),
			y27 => y27(i),
			y28 => y28(i),
			y29 => y29(i),
			y30 => y30(i),
			y31 => y31(i),
			y32 => y32(i),
			y33 => y33(i),
			y34 => y34(i),
			y35 => y35(i),
			y36 => y36(i),
			y37 => y37(i),
			y38 => y38(i),
			y39 => y39(i),
			y40 => y40(i),
			y41 => y41(i),
			y42 => y42(i),
			y43 => y43(i),
			y44 => y44(i),
			y45 => y45(i),
			y46 => y46(i),
			y47 => y47(i),
			y48 => y48(i),
			y49 => y49(i),
			y50 => y50(i),
			y51 => y51(i),
			y52 => y52(i),
			y53 => y53(i),
			y54 => y54(i),
			y55 => y55(i),
			y56 => y56(i),
			y57 => y57(i),
			y58 => y58(i),
			y59 => y59(i),
			y60 => y60(i),
			y61 => y61(i),
			y62 => y62(i),
			y63 => y63(i),
			address => address
		);
	end generate;
end;

library	ieee;
use	ieee.std_logic_1164.all;

entity decoder1x32 is
port(
	data:		in	std_logic;
	y0:			out	std_logic;
	y1:			out	std_logic;
	y2:			out	std_logic;
	y3:			out	std_logic;
	y4:			out	std_logic;
	y5:			out	std_logic;
	y6:			out	std_logic;
	y7:			out	std_logic;
	y8:			out	std_logic;
	y9:			out	std_logic;
	y10:		out	std_logic;
	y11:		out	std_logic;
	y12:		out	std_logic;
	y13:		out	std_logic;
	y14:		out	std_logic;
	y15:		out	std_logic;
	y16:		out	std_logic;
	y17:		out	std_logic;
	y18:		out	std_logic;
	y19:		out	std_logic;
	y20:		out	std_logic;
	y21:		out	std_logic;
	y22:		out	std_logic;
	y23:		out	std_logic;
	y24:		out	std_logic;
	y25:		out	std_logic;
	y26:		out	std_logic;
	y27:		out	std_logic;
	y28:		out	std_logic;
	y29:		out	std_logic;
	y30:		out	std_logic;
	y31:		out	std_logic;
	address:	in	std_logic_vector(4 downto 0)
);
end;

-- tested 2015/12/25 as part of decoderNx32 with modelsim, works.

architecture struct_decoder1x32 of decoder1x32 is
begin
	with address select y0	<= data when "00000", '0' when others;
	with address select y1	<= data when "00001", '0' when others;
	with address select y2	<= data when "00010", '0' when others;
	with address select y3	<= data when "00011", '0' when others;
	with address select y4	<= data when "00100", '0' when others;
	with address select y5	<= data when "00101", '0' when others;
	with address select y6	<= data when "00110", '0' when others;
	with address select y7	<= data when "00111", '0' when others;
	with address select y8	<= data when "01000", '0' when others;
	with address select y9	<= data when "01001", '0' when others;
	with address select y10	<= data when "01010", '0' when others;
	with address select y11	<= data when "01011", '0' when others;
	with address select y12	<= data when "01100", '0' when others;
	with address select y13	<= data when "01101", '0' when others;
	with address select y14	<= data when "01110", '0' when others;
	with address select y15	<= data when "01111", '0' when others;
	with address select y16	<= data when "10000", '0' when others;
	with address select y17	<= data when "10001", '0' when others;
	with address select y18	<= data when "10010", '0' when others;
	with address select y19	<= data when "10011", '0' when others;
	with address select y20	<= data when "10100", '0' when others;
	with address select y21	<= data when "10101", '0' when others;
	with address select y22	<= data when "10110", '0' when others;
	with address select y23	<= data when "10111", '0' when others;
	with address select y24	<= data when "11000", '0' when others;
	with address select y25	<= data when "11001", '0' when others;
	with address select y26	<= data when "11010", '0' when others;
	with address select y27	<= data when "11011", '0' when others;
	with address select y28	<= data when "11100", '0' when others;
	with address select y29	<= data when "11101", '0' when others;
	with address select y30	<= data when "11110", '0' when others;
	with address select y31	<= data when "11111", '0' when others;
end;

library	ieee;
use	ieee.std_logic_1164.all;

entity decoderNx32 is
generic(
	N:	positive
);
port(
	data:		in	std_logic_vector((N-1) downto 0);
	y0:			out	std_logic_vector((N-1) downto 0);
	y1:			out	std_logic_vector((N-1) downto 0);
	y2:			out	std_logic_vector((N-1) downto 0);
	y3:			out	std_logic_vector((N-1) downto 0);
	y4:			out	std_logic_vector((N-1) downto 0);
	y5:			out	std_logic_vector((N-1) downto 0);
	y6:			out	std_logic_vector((N-1) downto 0);
	y7:			out	std_logic_vector((N-1) downto 0);
	y8:			out	std_logic_vector((N-1) downto 0);
	y9:			out	std_logic_vector((N-1) downto 0);
	y10:		out	std_logic_vector((N-1) downto 0);
	y11:		out	std_logic_vector((N-1) downto 0);
	y12:		out	std_logic_vector((N-1) downto 0);
	y13:		out	std_logic_vector((N-1) downto 0);
	y14:		out	std_logic_vector((N-1) downto 0);
	y15:		out	std_logic_vector((N-1) downto 0);
	y16:		out	std_logic_vector((N-1) downto 0);
	y17:		out	std_logic_vector((N-1) downto 0);
	y18:		out	std_logic_vector((N-1) downto 0);
	y19:		out	std_logic_vector((N-1) downto 0);
	y20:		out	std_logic_vector((N-1) downto 0);
	y21:		out	std_logic_vector((N-1) downto 0);
	y22:		out	std_logic_vector((N-1) downto 0);
	y23:		out	std_logic_vector((N-1) downto 0);
	y24:		out	std_logic_vector((N-1) downto 0);
	y25:		out	std_logic_vector((N-1) downto 0);
	y26:		out	std_logic_vector((N-1) downto 0);
	y27:		out	std_logic_vector((N-1) downto 0);
	y28:		out	std_logic_vector((N-1) downto 0);
	y29:		out	std_logic_vector((N-1) downto 0);
	y30:		out	std_logic_vector((N-1) downto 0);
	y31:		out	std_logic_vector((N-1) downto 0);
	address:	in	std_logic_vector(4 downto 0)
);
end;

-- tested 2015/12/25 with modelsim and N = 8, works.
-- reverified with correct report statements 2015/12/26,  works.

architecture struct_decoderNx32 of decoderNx32 is
component decoder1x32 is
port(
	data:		in	std_logic;
	y0:			out	std_logic;
	y1:			out	std_logic;
	y2:			out	std_logic;
	y3:			out	std_logic;
	y4:			out	std_logic;
	y5:			out	std_logic;
	y6:			out	std_logic;
	y7:			out	std_logic;
	y8:			out	std_logic;
	y9:			out	std_logic;
	y10:		out	std_logic;
	y11:		out	std_logic;
	y12:		out	std_logic;
	y13:		out	std_logic;
	y14:		out	std_logic;
	y15:		out	std_logic;
	y16:		out	std_logic;
	y17:		out	std_logic;
	y18:		out	std_logic;
	y19:		out	std_logic;
	y20:		out	std_logic;
	y21:		out	std_logic;
	y22:		out	std_logic;
	y23:		out	std_logic;
	y24:		out	std_logic;
	y25:		out	std_logic;
	y26:		out	std_logic;
	y27:		out	std_logic;
	y28:		out	std_logic;
	y29:		out	std_logic;
	y30:		out	std_logic;
	y31:		out	std_logic;
	address:	in	std_logic_vector(4 downto 0)
);
end component;

begin
	u1:	for i in 0 to (N-1) generate
		u: decoder1x32 port map(
			data => data(i),
			y0 => y0(i),
			y1 => y1(i),
			y2 => y2(i),
			y3 => y3(i),
			y4 => y4(i),
			y5 => y5(i),
			y6 => y6(i),
			y7 => y7(i),
			y8 => y8(i),
			y9 => y9(i),
			y10	=> y10(i),
			y11	=> y11(i),
			y12	=> y12(i),
			y13	=> y13(i),
			y14	=> y14(i),
			y15	=> y15(i),
			y16	=> y16(i),
			y17	=> y17(i),
			y18	=> y18(i),
			y19	=> y19(i),
			y20	=> y20(i),
			y21	=> y21(i),
			y22	=> y22(i),
			y23	=> y23(i),
			y24	=> y24(i),
			y25	=> y25(i),
			y26	=> y26(i),
			y27	=> y27(i),
			y28	=> y28(i),
			y29	=> y29(i),
			y30	=> y30(i),
			y31	=> y31(i),
			address => address
		);
	end generate u1;
end;

library	ieee;
use	ieee.std_logic_1164.all;

entity encoder128x1 is
port(
	data0:			in	std_logic;
	data1:			in	std_logic;
	data2:			in	std_logic;
	data3:			in	std_logic;
	data4:			in	std_logic;
	data5:			in	std_logic;
	data6:			in	std_logic;
	data7:			in	std_logic;
	data8:			in	std_logic;
	data9:			in	std_logic;
	data10:			in	std_logic;
	data11:			in	std_logic;
	data12:			in	std_logic;
	data13:			in	std_logic;
	data14:			in	std_logic;
	data15:			in	std_logic;
	data16:			in	std_logic;
	data17:			in	std_logic;
	data18:			in	std_logic;
	data19:			in	std_logic;
	data20:			in	std_logic;
	data21:			in	std_logic;
	data22:			in	std_logic;
	data23:			in	std_logic;
	data24:			in	std_logic;
	data25:			in	std_logic;
	data26:			in	std_logic;
	data27:			in	std_logic;
	data28:			in	std_logic;
	data29:			in	std_logic;
	data30:			in	std_logic;
	data31:			in	std_logic;
	data32:			in	std_logic;
	data33:			in	std_logic;
	data34:			in	std_logic;
	data35:			in	std_logic;
	data36:			in	std_logic;
	data37:			in	std_logic;
	data38:			in	std_logic;
	data39:			in	std_logic;
	data40:			in	std_logic;
	data41:			in	std_logic;
	data42:			in	std_logic;
	data43:			in	std_logic;
	data44:			in	std_logic;
	data45:			in	std_logic;
	data46:			in	std_logic;
	data47:			in	std_logic;
	data48:			in	std_logic;
	data49:			in	std_logic;
	data50:			in	std_logic;
	data51:			in	std_logic;
	data52:			in	std_logic;
	data53:			in	std_logic;
	data54:			in	std_logic;
	data55:			in	std_logic;
	data56:			in	std_logic;
	data57:			in	std_logic;
	data58:			in	std_logic;
	data59:			in	std_logic;
	data60:			in	std_logic;
	data61:			in	std_logic;
	data62:			in	std_logic;
	data63:			in	std_logic;
	data64:			in	std_logic;
	data65:			in	std_logic;
	data66:			in	std_logic;
	data67:			in	std_logic;
	data68:			in	std_logic;
	data69:			in	std_logic;
	data70:			in	std_logic;
	data71:			in	std_logic;
	data72:			in	std_logic;
	data73:			in	std_logic;
	data74:			in	std_logic;
	data75:			in	std_logic;
	data76:			in	std_logic;
	data77:			in	std_logic;
	data78:			in	std_logic;
	data79:			in	std_logic;
	data80:			in	std_logic;
	data81:			in	std_logic;
	data82:			in	std_logic;
	data83:			in	std_logic;
	data84:			in	std_logic;
	data85:			in	std_logic;
	data86:			in	std_logic;
	data87:			in	std_logic;
	data88:			in	std_logic;
	data89:			in	std_logic;
	data90:			in	std_logic;
	data91:			in	std_logic;
	data92:			in	std_logic;
	data93:			in	std_logic;
	data94:			in	std_logic;
	data95:			in	std_logic;
	data96:			in	std_logic;
	data97:			in	std_logic;
	data98:			in	std_logic;
	data99:			in	std_logic;
	data100:		in	std_logic;
	data101:		in	std_logic;
	data102:		in	std_logic;
	data103:		in	std_logic;
	data104:		in	std_logic;
	data105:		in	std_logic;
	data106:		in	std_logic;
	data107:		in	std_logic;
	data108:		in	std_logic;
	data109:		in	std_logic;
	data110:		in	std_logic;
	data111:		in	std_logic;
	data112:		in	std_logic;
	data113:		in	std_logic;
	data114:		in	std_logic;
	data115:		in	std_logic;
	data116:		in	std_logic;
	data117:		in	std_logic;
	data118:		in	std_logic;
	data119:		in	std_logic;
	data120:		in	std_logic;
	data121:		in	std_logic;
	data122:		in	std_logic;
	data123:		in	std_logic;
	data124:		in	std_logic;
	data125:		in	std_logic;
	data126:		in	std_logic;
	data127:		in	std_logic;
	address:		in	std_logic_vector(6 downto 0);
	output:			out	std_logic
);
end;

architecture struct_encoder128x1 of encoder128x1 is
begin
	with address select
		output <=
			data0	when "0000000",
			data1	when "0000001",
			data2	when "0000010",
			data3	when "0000011",
			data4	when "0000100",
			data5	when "0000101",
			data6	when "0000110",
			data7	when "0000111",
			data8	when "0001000",
			data9	when "0001001",
			data10	when "0001010",
			data11	when "0001011",
			data12	when "0001100",
			data13	when "0001101",
			data14	when "0001110",
			data15	when "0001111",
			data16	when "0010000",
			data17	when "0010001",
			data18	when "0010010",
			data19	when "0010011",
			data20	when "0010100",
			data21	when "0010101",
			data22	when "0010110",
			data23	when "0010111",
			data24	when "0011000",
			data25	when "0011001",
			data26	when "0011010",
			data27	when "0011011",
			data28	when "0011100",
			data29	when "0011101",
			data30	when "0011110",
			data31	when "0011111",
			data32	when "0100000",
			data33	when "0100001",
			data34	when "0100010",
			data35	when "0100011",
			data36	when "0100100",
			data37	when "0100101",
			data38	when "0100110",
			data39	when "0100111",
			data40	when "0101000",
			data41	when "0101001",
			data42	when "0101010",
			data43	when "0101011",
			data44	when "0101100",
			data45	when "0101101",
			data46	when "0101110",
			data47	when "0101111",
			data48	when "0110000",
			data49	when "0110001",
			data50	when "0110010",
			data51	when "0110011",
			data52	when "0110100",
			data53	when "0110101",
			data54	when "0110110",
			data55	when "0110111",
			data56	when "0111000",
			data57	when "0111001",
			data58	when "0111010",
			data59	when "0111011",
			data60	when "0111100",
			data61	when "0111101",
			data62	when "0111110",
			data63	when "0111111",
			data64	when "1000000",
			data65	when "1000001",
			data66	when "1000010",
			data67	when "1000011",
			data68	when "1000100",
			data69	when "1000101",
			data70	when "1000110",
			data71	when "1000111",
			data72	when "1001000",
			data73	when "1001001",
			data74	when "1001010",
			data75	when "1001011",
			data76	when "1001100",
			data77	when "1001101",
			data78	when "1001110",
			data79	when "1001111",
			data80	when "1010000",
			data81	when "1010001",
			data82	when "1010010",
			data83	when "1010011",
			data84	when "1010100",
			data85	when "1010101",
			data86	when "1010110",
			data87	when "1010111",
			data88	when "1011000",
			data89	when "1011001",
			data90	when "1011010",
			data91	when "1011011",
			data92	when "1011100",
			data93	when "1011101",
			data94	when "1011110",
			data95	when "1011111",
			data96	when "1100000",
			data97	when "1100001",
			data98	when "1100010",
			data99	when "1100011",
			data100	when "1100100",
			data101	when "1100101",
			data102	when "1100110",
			data103	when "1100111",
			data104	when "1101000",
			data105	when "1101001",
			data106	when "1101010",
			data107	when "1101011",
			data108	when "1101100",
			data109	when "1101101",
			data110	when "1101110",
			data111	when "1101111",
			data112	when "1110000",
			data113	when "1110001",
			data114	when "1110010",
			data115	when "1110011",
			data116	when "1110100",
			data117	when "1110101",
			data118	when "1110110",
			data119	when "1110111",
			data120	when "1111000",
			data121	when "1111001",
			data122	when "1111010",
			data123	when "1111011",
			data124	when "1111100",
			data125	when "1111101",
			data126	when "1111110",
			data127	when "1111111",
			'0' when others;
end;

library	ieee;
use	ieee.std_logic_1164.all;

entity encoder128xN is
generic(
	N: positive
);
port(
	data0:			in	std_logic_vector((N-1) downto 0);
	data1:			in	std_logic_vector((N-1) downto 0);
	data2:			in	std_logic_vector((N-1) downto 0);
	data3:			in	std_logic_vector((N-1) downto 0);
	data4:			in	std_logic_vector((N-1) downto 0);
	data5:			in	std_logic_vector((N-1) downto 0);
	data6:			in	std_logic_vector((N-1) downto 0);
	data7:			in	std_logic_vector((N-1) downto 0);
	data8:			in	std_logic_vector((N-1) downto 0);
	data9:			in	std_logic_vector((N-1) downto 0);
	data10:			in	std_logic_vector((N-1) downto 0);
	data11:			in	std_logic_vector((N-1) downto 0);
	data12:			in	std_logic_vector((N-1) downto 0);
	data13:			in	std_logic_vector((N-1) downto 0);
	data14:			in	std_logic_vector((N-1) downto 0);
	data15:			in	std_logic_vector((N-1) downto 0);
	data16:			in	std_logic_vector((N-1) downto 0);
	data17:			in	std_logic_vector((N-1) downto 0);
	data18:			in	std_logic_vector((N-1) downto 0);
	data19:			in	std_logic_vector((N-1) downto 0);
	data20:			in	std_logic_vector((N-1) downto 0);
	data21:			in	std_logic_vector((N-1) downto 0);
	data22:			in	std_logic_vector((N-1) downto 0);
	data23:			in	std_logic_vector((N-1) downto 0);
	data24:			in	std_logic_vector((N-1) downto 0);
	data25:			in	std_logic_vector((N-1) downto 0);
	data26:			in	std_logic_vector((N-1) downto 0);
	data27:			in	std_logic_vector((N-1) downto 0);
	data28:			in	std_logic_vector((N-1) downto 0);
	data29:			in	std_logic_vector((N-1) downto 0);
	data30:			in	std_logic_vector((N-1) downto 0);
	data31:			in	std_logic_vector((N-1) downto 0);
	data32:			in	std_logic_vector((N-1) downto 0);
	data33:			in	std_logic_vector((N-1) downto 0);
	data34:			in	std_logic_vector((N-1) downto 0);
	data35:			in	std_logic_vector((N-1) downto 0);
	data36:			in	std_logic_vector((N-1) downto 0);
	data37:			in	std_logic_vector((N-1) downto 0);
	data38:			in	std_logic_vector((N-1) downto 0);
	data39:			in	std_logic_vector((N-1) downto 0);
	data40:			in	std_logic_vector((N-1) downto 0);
	data41:			in	std_logic_vector((N-1) downto 0);
	data42:			in	std_logic_vector((N-1) downto 0);
	data43:			in	std_logic_vector((N-1) downto 0);
	data44:			in	std_logic_vector((N-1) downto 0);
	data45:			in	std_logic_vector((N-1) downto 0);
	data46:			in	std_logic_vector((N-1) downto 0);
	data47:			in	std_logic_vector((N-1) downto 0);
	data48:			in	std_logic_vector((N-1) downto 0);
	data49:			in	std_logic_vector((N-1) downto 0);
	data50:			in	std_logic_vector((N-1) downto 0);
	data51:			in	std_logic_vector((N-1) downto 0);
	data52:			in	std_logic_vector((N-1) downto 0);
	data53:			in	std_logic_vector((N-1) downto 0);
	data54:			in	std_logic_vector((N-1) downto 0);
	data55:			in	std_logic_vector((N-1) downto 0);
	data56:			in	std_logic_vector((N-1) downto 0);
	data57:			in	std_logic_vector((N-1) downto 0);
	data58:			in	std_logic_vector((N-1) downto 0);
	data59:			in	std_logic_vector((N-1) downto 0);
	data60:			in	std_logic_vector((N-1) downto 0);
	data61:			in	std_logic_vector((N-1) downto 0);
	data62:			in	std_logic_vector((N-1) downto 0);
	data63:			in	std_logic_vector((N-1) downto 0);
	data64:			in	std_logic_vector((N-1) downto 0);
	data65:			in	std_logic_vector((N-1) downto 0);
	data66:			in	std_logic_vector((N-1) downto 0);
	data67:			in	std_logic_vector((N-1) downto 0);
	data68:			in	std_logic_vector((N-1) downto 0);
	data69:			in	std_logic_vector((N-1) downto 0);
	data70:			in	std_logic_vector((N-1) downto 0);
	data71:			in	std_logic_vector((N-1) downto 0);
	data72:			in	std_logic_vector((N-1) downto 0);
	data73:			in	std_logic_vector((N-1) downto 0);
	data74:			in	std_logic_vector((N-1) downto 0);
	data75:			in	std_logic_vector((N-1) downto 0);
	data76:			in	std_logic_vector((N-1) downto 0);
	data77:			in	std_logic_vector((N-1) downto 0);
	data78:			in	std_logic_vector((N-1) downto 0);
	data79:			in	std_logic_vector((N-1) downto 0);
	data80:			in	std_logic_vector((N-1) downto 0);
	data81:			in	std_logic_vector((N-1) downto 0);
	data82:			in	std_logic_vector((N-1) downto 0);
	data83:			in	std_logic_vector((N-1) downto 0);
	data84:			in	std_logic_vector((N-1) downto 0);
	data85:			in	std_logic_vector((N-1) downto 0);
	data86:			in	std_logic_vector((N-1) downto 0);
	data87:			in	std_logic_vector((N-1) downto 0);
	data88:			in	std_logic_vector((N-1) downto 0);
	data89:			in	std_logic_vector((N-1) downto 0);
	data90:			in	std_logic_vector((N-1) downto 0);
	data91:			in	std_logic_vector((N-1) downto 0);
	data92:			in	std_logic_vector((N-1) downto 0);
	data93:			in	std_logic_vector((N-1) downto 0);
	data94:			in	std_logic_vector((N-1) downto 0);
	data95:			in	std_logic_vector((N-1) downto 0);
	data96:			in	std_logic_vector((N-1) downto 0);
	data97:			in	std_logic_vector((N-1) downto 0);
	data98:			in	std_logic_vector((N-1) downto 0);
	data99:			in	std_logic_vector((N-1) downto 0);
	data100:		in	std_logic_vector((N-1) downto 0);
	data101:		in	std_logic_vector((N-1) downto 0);
	data102:		in	std_logic_vector((N-1) downto 0);
	data103:		in	std_logic_vector((N-1) downto 0);
	data104:		in	std_logic_vector((N-1) downto 0);
	data105:		in	std_logic_vector((N-1) downto 0);
	data106:		in	std_logic_vector((N-1) downto 0);
	data107:		in	std_logic_vector((N-1) downto 0);
	data108:		in	std_logic_vector((N-1) downto 0);
	data109:		in	std_logic_vector((N-1) downto 0);
	data110:		in	std_logic_vector((N-1) downto 0);
	data111:		in	std_logic_vector((N-1) downto 0);
	data112:		in	std_logic_vector((N-1) downto 0);
	data113:		in	std_logic_vector((N-1) downto 0);
	data114:		in	std_logic_vector((N-1) downto 0);
	data115:		in	std_logic_vector((N-1) downto 0);
	data116:		in	std_logic_vector((N-1) downto 0);
	data117:		in	std_logic_vector((N-1) downto 0);
	data118:		in	std_logic_vector((N-1) downto 0);
	data119:		in	std_logic_vector((N-1) downto 0);
	data120:		in	std_logic_vector((N-1) downto 0);
	data121:		in	std_logic_vector((N-1) downto 0);
	data122:		in	std_logic_vector((N-1) downto 0);
	data123:		in	std_logic_vector((N-1) downto 0);
	data124:		in	std_logic_vector((N-1) downto 0);
	data125:		in	std_logic_vector((N-1) downto 0);
	data126:		in	std_logic_vector((N-1) downto 0);
	data127:		in	std_logic_vector((N-1) downto 0);
	address:		in	std_logic_vector(6 downto 0);
	output:			out	std_logic_vector((N-1) downto 0)
);
end;

architecture struct_encoder128xN of encoder128xN is
component encoder128x1 is
port(
	data0:			in	std_logic;
	data1:			in	std_logic;
	data2:			in	std_logic;
	data3:			in	std_logic;
	data4:			in	std_logic;
	data5:			in	std_logic;
	data6:			in	std_logic;
	data7:			in	std_logic;
	data8:			in	std_logic;
	data9:			in	std_logic;
	data10:			in	std_logic;
	data11:			in	std_logic;
	data12:			in	std_logic;
	data13:			in	std_logic;
	data14:			in	std_logic;
	data15:			in	std_logic;
	data16:			in	std_logic;
	data17:			in	std_logic;
	data18:			in	std_logic;
	data19:			in	std_logic;
	data20:			in	std_logic;
	data21:			in	std_logic;
	data22:			in	std_logic;
	data23:			in	std_logic;
	data24:			in	std_logic;
	data25:			in	std_logic;
	data26:			in	std_logic;
	data27:			in	std_logic;
	data28:			in	std_logic;
	data29:			in	std_logic;
	data30:			in	std_logic;
	data31:			in	std_logic;
	data32:			in	std_logic;
	data33:			in	std_logic;
	data34:			in	std_logic;
	data35:			in	std_logic;
	data36:			in	std_logic;
	data37:			in	std_logic;
	data38:			in	std_logic;
	data39:			in	std_logic;
	data40:			in	std_logic;
	data41:			in	std_logic;
	data42:			in	std_logic;
	data43:			in	std_logic;
	data44:			in	std_logic;
	data45:			in	std_logic;
	data46:			in	std_logic;
	data47:			in	std_logic;
	data48:			in	std_logic;
	data49:			in	std_logic;
	data50:			in	std_logic;
	data51:			in	std_logic;
	data52:			in	std_logic;
	data53:			in	std_logic;
	data54:			in	std_logic;
	data55:			in	std_logic;
	data56:			in	std_logic;
	data57:			in	std_logic;
	data58:			in	std_logic;
	data59:			in	std_logic;
	data60:			in	std_logic;
	data61:			in	std_logic;
	data62:			in	std_logic;
	data63:			in	std_logic;
	data64:			in	std_logic;
	data65:			in	std_logic;
	data66:			in	std_logic;
	data67:			in	std_logic;
	data68:			in	std_logic;
	data69:			in	std_logic;
	data70:			in	std_logic;
	data71:			in	std_logic;
	data72:			in	std_logic;
	data73:			in	std_logic;
	data74:			in	std_logic;
	data75:			in	std_logic;
	data76:			in	std_logic;
	data77:			in	std_logic;
	data78:			in	std_logic;
	data79:			in	std_logic;
	data80:			in	std_logic;
	data81:			in	std_logic;
	data82:			in	std_logic;
	data83:			in	std_logic;
	data84:			in	std_logic;
	data85:			in	std_logic;
	data86:			in	std_logic;
	data87:			in	std_logic;
	data88:			in	std_logic;
	data89:			in	std_logic;
	data90:			in	std_logic;
	data91:			in	std_logic;
	data92:			in	std_logic;
	data93:			in	std_logic;
	data94:			in	std_logic;
	data95:			in	std_logic;
	data96:			in	std_logic;
	data97:			in	std_logic;
	data98:			in	std_logic;
	data99:			in	std_logic;
	data100:		in	std_logic;
	data101:		in	std_logic;
	data102:		in	std_logic;
	data103:		in	std_logic;
	data104:		in	std_logic;
	data105:		in	std_logic;
	data106:		in	std_logic;
	data107:		in	std_logic;
	data108:		in	std_logic;
	data109:		in	std_logic;
	data110:		in	std_logic;
	data111:		in	std_logic;
	data112:		in	std_logic;
	data113:		in	std_logic;
	data114:		in	std_logic;
	data115:		in	std_logic;
	data116:		in	std_logic;
	data117:		in	std_logic;
	data118:		in	std_logic;
	data119:		in	std_logic;
	data120:		in	std_logic;
	data121:		in	std_logic;
	data122:		in	std_logic;
	data123:		in	std_logic;
	data124:		in	std_logic;
	data125:		in	std_logic;
	data126:		in	std_logic;
	data127:		in	std_logic;
	address:		in	std_logic_vector(6 downto 0);
	output:			out	std_logic
);
end component;

begin
	u1:	for i in (N-1) downto 0 generate
		u: encoder128x1 port map(
			data0	=> data0(i),
			data1	=> data1(i),
			data2	=> data2(i),
			data3	=> data3(i),
			data4	=> data4(i),
			data5	=> data5(i),
			data6	=> data6(i),
			data7	=> data7(i),
			data8	=> data8(i),
			data9	=> data9(i),
			data10	=> data10(i),
			data11	=> data11(i),
			data12	=> data12(i),
			data13	=> data13(i),
			data14	=> data14(i),
			data15	=> data15(i),
			data16	=> data16(i),
			data17	=> data17(i),
			data18	=> data18(i),
			data19	=> data19(i),
			data20	=> data20(i),
			data21	=> data21(i),
			data22	=> data22(i),
			data23	=> data23(i),
			data24	=> data24(i),
			data25	=> data25(i),
			data26	=> data26(i),
			data27	=> data27(i),
			data28	=> data28(i),
			data29	=> data29(i),
			data30	=> data30(i),
			data31	=> data31(i),
			data32	=> data32(i),
			data33	=> data33(i),
			data34	=> data34(i),
			data35	=> data35(i),
			data36	=> data36(i),
			data37	=> data37(i),
			data38	=> data38(i),
			data39	=> data39(i),
			data40	=> data40(i),
			data41	=> data41(i),
			data42	=> data42(i),
			data43	=> data43(i),
			data44	=> data44(i),
			data45	=> data45(i),
			data46	=> data46(i),
			data47	=> data47(i),
			data48	=> data48(i),
			data49	=> data49(i),
			data50	=> data50(i),
			data51	=> data51(i),
			data52	=> data52(i),
			data53	=> data53(i),
			data54	=> data54(i),
			data55	=> data55(i),
			data56	=> data56(i),
			data57	=> data57(i),
			data58	=> data58(i),
			data59	=> data59(i),
			data60	=> data60(i),
			data61	=> data61(i),
			data62	=> data62(i),
			data63	=> data63(i),
			data64	=> data64(i),
			data65	=> data65(i),
			data66	=> data66(i),
			data67	=> data67(i),
			data68	=> data68(i),
			data69	=> data69(i),
			data70	=> data70(i),
			data71	=> data71(i),
			data72	=> data72(i),
			data73	=> data73(i),
			data74	=> data74(i),
			data75	=> data75(i),
			data76	=> data76(i),
			data77	=> data77(i),
			data78	=> data78(i),
			data79	=> data79(i),
			data80	=> data80(i),
			data81	=> data81(i),
			data82	=> data82(i),
			data83	=> data83(i),
			data84	=> data84(i),
			data85	=> data85(i),
			data86	=> data86(i),
			data87	=> data87(i),
			data88	=> data88(i),
			data89	=> data89(i),
			data90	=> data90(i),
			data91	=> data91(i),
			data92	=> data92(i),
			data93	=> data93(i),
			data94	=> data94(i),
			data95	=> data95(i),
			data96	=> data96(i),
			data97	=> data97(i),
			data98	=> data98(i),
			data99	=> data99(i),
			data100	=> data100(i),
			data101	=> data101(i),
			data102	=> data102(i),
			data103	=> data103(i),
			data104	=> data104(i),
			data105	=> data105(i),
			data106	=> data106(i),
			data107	=> data107(i),
			data108	=> data108(i),
			data109	=> data109(i),
			data110	=> data110(i),
			data111	=> data111(i),
			data112	=> data112(i),
			data113	=> data113(i),
			data114	=> data114(i),
			data115	=> data115(i),
			data116	=> data116(i),
			data117	=> data117(i),
			data118	=> data118(i),
			data119	=> data119(i),
			data120	=> data120(i),
			data121	=> data121(i),
			data122	=> data122(i),
			data123	=> data123(i),
			data124	=> data124(i),
			data125	=> data125(i),
			data126	=> data126(i),
			data127	=> data127(i),
			address	=> address,
			output => output(i)
		);
	end generate u1;
end;

library	ieee;
use	ieee.std_logic_1164.all;

entity decoder1x128 is
port(
	data:		in	std_logic;
	y0:			out	std_logic;
	y1:			out	std_logic;
	y2:			out	std_logic;
	y3:			out	std_logic;
	y4:			out	std_logic;
	y5:			out	std_logic;
	y6:			out	std_logic;
	y7:			out	std_logic;
	y8:			out	std_logic;
	y9:			out	std_logic;
	y10:		out	std_logic;
	y11:		out	std_logic;
	y12:		out	std_logic;
	y13:		out	std_logic;
	y14:		out	std_logic;
	y15:		out	std_logic;
	y16:		out	std_logic;
	y17:		out	std_logic;
	y18:		out	std_logic;
	y19:		out	std_logic;
	y20:		out	std_logic;
	y21:		out	std_logic;
	y22:		out	std_logic;
	y23:		out	std_logic;
	y24:		out	std_logic;
	y25:		out	std_logic;
	y26:		out	std_logic;
	y27:		out	std_logic;
	y28:		out	std_logic;
	y29:		out	std_logic;
	y30:		out	std_logic;
	y31:		out	std_logic;
	y32:		out	std_logic;
	y33:		out	std_logic;
	y34:		out	std_logic;
	y35:		out	std_logic;
	y36:		out	std_logic;
	y37:		out	std_logic;
	y38:		out	std_logic;
	y39:		out	std_logic;
	y40:		out	std_logic;
	y41:		out	std_logic;
	y42:		out	std_logic;
	y43:		out	std_logic;
	y44:		out	std_logic;
	y45:		out	std_logic;
	y46:		out	std_logic;
	y47:		out	std_logic;
	y48:		out	std_logic;
	y49:		out	std_logic;
	y50:		out	std_logic;
	y51:		out	std_logic;
	y52:		out	std_logic;
	y53:		out	std_logic;
	y54:		out	std_logic;
	y55:		out	std_logic;
	y56:		out	std_logic;
	y57:		out	std_logic;
	y58:		out	std_logic;
	y59:		out	std_logic;
	y60:		out	std_logic;
	y61:		out	std_logic;
	y62:		out	std_logic;
	y63:		out	std_logic;
	y64:		out	std_logic;
	y65:		out	std_logic;
	y66:		out	std_logic;
	y67:		out	std_logic;
	y68:		out	std_logic;
	y69:		out	std_logic;
	y70:		out	std_logic;
	y71:		out	std_logic;
	y72:		out	std_logic;
	y73:		out	std_logic;
	y74:		out	std_logic;
	y75:		out	std_logic;
	y76:		out	std_logic;
	y77:		out	std_logic;
	y78:		out	std_logic;
	y79:		out	std_logic;
	y80:		out	std_logic;
	y81:		out	std_logic;
	y82:		out	std_logic;
	y83:		out	std_logic;
	y84:		out	std_logic;
	y85:		out	std_logic;
	y86:		out	std_logic;
	y87:		out	std_logic;
	y88:		out	std_logic;
	y89:		out	std_logic;
	y90:		out	std_logic;
	y91:		out	std_logic;
	y92:		out	std_logic;
	y93:		out	std_logic;
	y94:		out	std_logic;
	y95:		out	std_logic;
	y96:		out	std_logic;
	y97:		out	std_logic;
	y98:		out	std_logic;
	y99:		out	std_logic;
	y100:		out	std_logic;
	y101:		out	std_logic;
	y102:		out	std_logic;
	y103:		out	std_logic;
	y104:		out	std_logic;
	y105:		out	std_logic;
	y106:		out	std_logic;
	y107:		out	std_logic;
	y108:		out	std_logic;
	y109:		out	std_logic;
	y110:		out	std_logic;
	y111:		out	std_logic;
	y112:		out	std_logic;
	y113:		out	std_logic;
	y114:		out	std_logic;
	y115:		out	std_logic;
	y116:		out	std_logic;
	y117:		out	std_logic;
	y118:		out	std_logic;
	y119:		out	std_logic;
	y120:		out	std_logic;
	y121:		out	std_logic;
	y122:		out	std_logic;
	y123:		out	std_logic;
	y124:		out	std_logic;
	y125:		out	std_logic;
	y126:		out	std_logic;
	y127:		out	std_logic;
	address:	in	std_logic_vector(6 downto 0)
);
end;

architecture struct_decoder1x128 of decoder1x128 is
begin
	y0		<= data when address = "0000000" else '0';
	y1		<= data when address = "0000001" else '0';
	y2		<= data when address = "0000010" else '0';
	y3		<= data when address = "0000011" else '0';
	y4		<= data when address = "0000100" else '0';
	y5		<= data when address = "0000101" else '0';
	y6		<= data when address = "0000110" else '0';
	y7		<= data when address = "0000111" else '0';
	y8		<= data when address = "0001000" else '0';
	y9		<= data when address = "0001001" else '0';
	y10		<= data when address = "0001010" else '0';
	y11		<= data when address = "0001011" else '0';
	y12		<= data when address = "0001100" else '0';
	y13		<= data when address = "0001101" else '0';
	y14		<= data when address = "0001110" else '0';
	y15		<= data when address = "0001111" else '0';
	y16		<= data when address = "0010000" else '0';
	y17		<= data when address = "0010001" else '0';
	y18		<= data when address = "0010010" else '0';
	y19		<= data when address = "0010011" else '0';
	y20		<= data when address = "0010100" else '0';
	y21		<= data when address = "0010101" else '0';
	y22		<= data when address = "0010110" else '0';
	y23		<= data when address = "0010111" else '0';
	y24		<= data when address = "0011000" else '0';
	y25		<= data when address = "0011001" else '0';
	y26		<= data when address = "0011010" else '0';
	y27		<= data when address = "0011011" else '0';
	y28		<= data when address = "0011100" else '0';
	y29		<= data when address = "0011101" else '0';
	y30		<= data when address = "0011110" else '0';
	y31		<= data when address = "0011111" else '0';
	y32		<= data when address = "0100000" else '0';
	y33		<= data when address = "0100001" else '0';
	y34		<= data when address = "0100010" else '0';
	y35		<= data when address = "0100011" else '0';
	y36		<= data when address = "0100100" else '0';
	y37		<= data when address = "0100101" else '0';
	y38		<= data when address = "0100110" else '0';
	y39		<= data when address = "0100111" else '0';
	y40		<= data when address = "0101000" else '0';
	y41		<= data when address = "0101001" else '0';
	y42		<= data when address = "0101010" else '0';
	y43		<= data when address = "0101011" else '0';
	y44		<= data when address = "0101100" else '0';
	y45		<= data when address = "0101101" else '0';
	y46		<= data when address = "0101110" else '0';
	y47		<= data when address = "0101111" else '0';
	y48		<= data when address = "0110000" else '0';
	y49		<= data when address = "0110001" else '0';
	y50		<= data when address = "0110010" else '0';
	y51		<= data when address = "0110011" else '0';
	y52		<= data when address = "0110100" else '0';
	y53		<= data when address = "0110101" else '0';
	y54		<= data when address = "0110110" else '0';
	y55		<= data when address = "0110111" else '0';
	y56		<= data when address = "0111000" else '0';
	y57		<= data when address = "0111001" else '0';
	y58		<= data when address = "0111010" else '0';
	y59		<= data when address = "0111011" else '0';
	y60		<= data when address = "0111100" else '0';
	y61		<= data when address = "0111101" else '0';
	y62		<= data when address = "0111110" else '0';
	y63		<= data when address = "0111111" else '0';
	y64		<= data when address = "1000000" else '0';
	y65		<= data when address = "1000001" else '0';
	y66		<= data when address = "1000010" else '0';
	y67		<= data when address = "1000011" else '0';
	y68		<= data when address = "1000100" else '0';
	y69		<= data when address = "1000101" else '0';
	y70		<= data when address = "1000110" else '0';
	y71		<= data when address = "1000111" else '0';
	y72		<= data when address = "1001000" else '0';
	y73		<= data when address = "1001001" else '0';
	y74		<= data when address = "1001010" else '0';
	y75		<= data when address = "1001011" else '0';
	y76		<= data when address = "1001100" else '0';
	y77		<= data when address = "1001101" else '0';
	y78		<= data when address = "1001110" else '0';
	y79		<= data when address = "1001111" else '0';
	y80		<= data when address = "1010000" else '0';
	y81		<= data when address = "1010001" else '0';
	y82		<= data when address = "1010010" else '0';
	y83		<= data when address = "1010011" else '0';
	y84		<= data when address = "1010100" else '0';
	y85		<= data when address = "1010101" else '0';
	y86		<= data when address = "1010110" else '0';
	y87		<= data when address = "1010111" else '0';
	y88		<= data when address = "1011000" else '0';
	y89		<= data when address = "1011001" else '0';
	y90		<= data when address = "1011010" else '0';
	y91		<= data when address = "1011011" else '0';
	y92		<= data when address = "1011100" else '0';
	y93		<= data when address = "1011101" else '0';
	y94		<= data when address = "1011110" else '0';
	y95		<= data when address = "1011111" else '0';
	y96		<= data when address = "1100000" else '0';
	y97		<= data when address = "1100001" else '0';
	y98		<= data when address = "1100010" else '0';
	y99		<= data when address = "1100011" else '0';
	y100	<= data when address = "1100100" else '0';
	y101	<= data when address = "1100101" else '0';
	y102	<= data when address = "1100110" else '0';
	y103	<= data when address = "1100111" else '0';
	y104	<= data when address = "1101000" else '0';
	y105	<= data when address = "1101001" else '0';
	y106	<= data when address = "1101010" else '0';
	y107	<= data when address = "1101011" else '0';
	y108	<= data when address = "1101100" else '0';
	y109	<= data when address = "1101101" else '0';
	y110	<= data when address = "1101110" else '0';
	y111	<= data when address = "1101111" else '0';
	y112	<= data when address = "1110000" else '0';
	y113	<= data when address = "1110001" else '0';
	y114	<= data when address = "1110010" else '0';
	y115	<= data when address = "1110011" else '0';
	y116	<= data when address = "1110100" else '0';
	y117	<= data when address = "1110101" else '0';
	y118	<= data when address = "1110110" else '0';
	y119	<= data when address = "1110111" else '0';
	y120	<= data when address = "1111000" else '0';
	y121	<= data when address = "1111001" else '0';
	y122	<= data when address = "1111010" else '0';
	y123	<= data when address = "1111011" else '0';
	y124	<= data when address = "1111100" else '0';
	y125	<= data when address = "1111101" else '0';
	y126	<= data when address = "1111110" else '0';
	y127	<= data when address = "1111111" else '0';
end;

library	ieee;
use	ieee.std_logic_1164.all;

entity decoderNx128 is
generic(
	N: positive
);
port(
	data:		in	std_logic_vector((N-1) downto 0);
	y0:			out	std_logic_vector((N-1) downto 0);
	y1:			out	std_logic_vector((N-1) downto 0);
	y2:			out	std_logic_vector((N-1) downto 0);
	y3:			out	std_logic_vector((N-1) downto 0);
	y4:			out	std_logic_vector((N-1) downto 0);
	y5:			out	std_logic_vector((N-1) downto 0);
	y6:			out	std_logic_vector((N-1) downto 0);
	y7:			out	std_logic_vector((N-1) downto 0);
	y8:			out	std_logic_vector((N-1) downto 0);
	y9:			out	std_logic_vector((N-1) downto 0);
	y10:		out	std_logic_vector((N-1) downto 0);
	y11:		out	std_logic_vector((N-1) downto 0);
	y12:		out	std_logic_vector((N-1) downto 0);
	y13:		out	std_logic_vector((N-1) downto 0);
	y14:		out	std_logic_vector((N-1) downto 0);
	y15:		out	std_logic_vector((N-1) downto 0);
	y16:		out	std_logic_vector((N-1) downto 0);
	y17:		out	std_logic_vector((N-1) downto 0);
	y18:		out	std_logic_vector((N-1) downto 0);
	y19:		out	std_logic_vector((N-1) downto 0);
	y20:		out	std_logic_vector((N-1) downto 0);
	y21:		out	std_logic_vector((N-1) downto 0);
	y22:		out	std_logic_vector((N-1) downto 0);
	y23:		out	std_logic_vector((N-1) downto 0);
	y24:		out	std_logic_vector((N-1) downto 0);
	y25:		out	std_logic_vector((N-1) downto 0);
	y26:		out	std_logic_vector((N-1) downto 0);
	y27:		out	std_logic_vector((N-1) downto 0);
	y28:		out	std_logic_vector((N-1) downto 0);
	y29:		out	std_logic_vector((N-1) downto 0);
	y30:		out	std_logic_vector((N-1) downto 0);
	y31:		out	std_logic_vector((N-1) downto 0);
	y32:		out	std_logic_vector((N-1) downto 0);
	y33:		out	std_logic_vector((N-1) downto 0);
	y34:		out	std_logic_vector((N-1) downto 0);
	y35:		out	std_logic_vector((N-1) downto 0);
	y36:		out	std_logic_vector((N-1) downto 0);
	y37:		out	std_logic_vector((N-1) downto 0);
	y38:		out	std_logic_vector((N-1) downto 0);
	y39:		out	std_logic_vector((N-1) downto 0);
	y40:		out	std_logic_vector((N-1) downto 0);
	y41:		out	std_logic_vector((N-1) downto 0);
	y42:		out	std_logic_vector((N-1) downto 0);
	y43:		out	std_logic_vector((N-1) downto 0);
	y44:		out	std_logic_vector((N-1) downto 0);
	y45:		out	std_logic_vector((N-1) downto 0);
	y46:		out	std_logic_vector((N-1) downto 0);
	y47:		out	std_logic_vector((N-1) downto 0);
	y48:		out	std_logic_vector((N-1) downto 0);
	y49:		out	std_logic_vector((N-1) downto 0);
	y50:		out	std_logic_vector((N-1) downto 0);
	y51:		out	std_logic_vector((N-1) downto 0);
	y52:		out	std_logic_vector((N-1) downto 0);
	y53:		out	std_logic_vector((N-1) downto 0);
	y54:		out	std_logic_vector((N-1) downto 0);
	y55:		out	std_logic_vector((N-1) downto 0);
	y56:		out	std_logic_vector((N-1) downto 0);
	y57:		out	std_logic_vector((N-1) downto 0);
	y58:		out	std_logic_vector((N-1) downto 0);
	y59:		out	std_logic_vector((N-1) downto 0);
	y60:		out	std_logic_vector((N-1) downto 0);
	y61:		out	std_logic_vector((N-1) downto 0);
	y62:		out	std_logic_vector((N-1) downto 0);
	y63:		out	std_logic_vector((N-1) downto 0);
	y64:		out	std_logic_vector((N-1) downto 0);
	y65:		out	std_logic_vector((N-1) downto 0);
	y66:		out	std_logic_vector((N-1) downto 0);
	y67:		out	std_logic_vector((N-1) downto 0);
	y68:		out	std_logic_vector((N-1) downto 0);
	y69:		out	std_logic_vector((N-1) downto 0);
	y70:		out	std_logic_vector((N-1) downto 0);
	y71:		out	std_logic_vector((N-1) downto 0);
	y72:		out	std_logic_vector((N-1) downto 0);
	y73:		out	std_logic_vector((N-1) downto 0);
	y74:		out	std_logic_vector((N-1) downto 0);
	y75:		out	std_logic_vector((N-1) downto 0);
	y76:		out	std_logic_vector((N-1) downto 0);
	y77:		out	std_logic_vector((N-1) downto 0);
	y78:		out	std_logic_vector((N-1) downto 0);
	y79:		out	std_logic_vector((N-1) downto 0);
	y80:		out	std_logic_vector((N-1) downto 0);
	y81:		out	std_logic_vector((N-1) downto 0);
	y82:		out	std_logic_vector((N-1) downto 0);
	y83:		out	std_logic_vector((N-1) downto 0);
	y84:		out	std_logic_vector((N-1) downto 0);
	y85:		out	std_logic_vector((N-1) downto 0);
	y86:		out	std_logic_vector((N-1) downto 0);
	y87:		out	std_logic_vector((N-1) downto 0);
	y88:		out	std_logic_vector((N-1) downto 0);
	y89:		out	std_logic_vector((N-1) downto 0);
	y90:		out	std_logic_vector((N-1) downto 0);
	y91:		out	std_logic_vector((N-1) downto 0);
	y92:		out	std_logic_vector((N-1) downto 0);
	y93:		out	std_logic_vector((N-1) downto 0);
	y94:		out	std_logic_vector((N-1) downto 0);
	y95:		out	std_logic_vector((N-1) downto 0);
	y96:		out	std_logic_vector((N-1) downto 0);
	y97:		out	std_logic_vector((N-1) downto 0);
	y98:		out	std_logic_vector((N-1) downto 0);
	y99:		out	std_logic_vector((N-1) downto 0);
	y100:		out	std_logic_vector((N-1) downto 0);
	y101:		out	std_logic_vector((N-1) downto 0);
	y102:		out	std_logic_vector((N-1) downto 0);
	y103:		out	std_logic_vector((N-1) downto 0);
	y104:		out	std_logic_vector((N-1) downto 0);
	y105:		out	std_logic_vector((N-1) downto 0);
	y106:		out	std_logic_vector((N-1) downto 0);
	y107:		out	std_logic_vector((N-1) downto 0);
	y108:		out	std_logic_vector((N-1) downto 0);
	y109:		out	std_logic_vector((N-1) downto 0);
	y110:		out	std_logic_vector((N-1) downto 0);
	y111:		out	std_logic_vector((N-1) downto 0);
	y112:		out	std_logic_vector((N-1) downto 0);
	y113:		out	std_logic_vector((N-1) downto 0);
	y114:		out	std_logic_vector((N-1) downto 0);
	y115:		out	std_logic_vector((N-1) downto 0);
	y116:		out	std_logic_vector((N-1) downto 0);
	y117:		out	std_logic_vector((N-1) downto 0);
	y118:		out	std_logic_vector((N-1) downto 0);
	y119:		out	std_logic_vector((N-1) downto 0);
	y120:		out	std_logic_vector((N-1) downto 0);
	y121:		out	std_logic_vector((N-1) downto 0);
	y122:		out	std_logic_vector((N-1) downto 0);
	y123:		out	std_logic_vector((N-1) downto 0);
	y124:		out	std_logic_vector((N-1) downto 0);
	y125:		out	std_logic_vector((N-1) downto 0);
	y126:		out	std_logic_vector((N-1) downto 0);
	y127:		out	std_logic_vector((N-1) downto 0);
	address:	in	std_logic_vector(6 downto 0)
);
end;

architecture struct_decoderNx128 of decoderNx128 is
component decoder1x128 is
port(
	data:		in	std_logic;
	y0:			out	std_logic;
	y1:			out	std_logic;
	y2:			out	std_logic;
	y3:			out	std_logic;
	y4:			out	std_logic;
	y5:			out	std_logic;
	y6:			out	std_logic;
	y7:			out	std_logic;
	y8:			out	std_logic;
	y9:			out	std_logic;
	y10:		out	std_logic;
	y11:		out	std_logic;
	y12:		out	std_logic;
	y13:		out	std_logic;
	y14:		out	std_logic;
	y15:		out	std_logic;
	y16:		out	std_logic;
	y17:		out	std_logic;
	y18:		out	std_logic;
	y19:		out	std_logic;
	y20:		out	std_logic;
	y21:		out	std_logic;
	y22:		out	std_logic;
	y23:		out	std_logic;
	y24:		out	std_logic;
	y25:		out	std_logic;
	y26:		out	std_logic;
	y27:		out	std_logic;
	y28:		out	std_logic;
	y29:		out	std_logic;
	y30:		out	std_logic;
	y31:		out	std_logic;
	y32:		out	std_logic;
	y33:		out	std_logic;
	y34:		out	std_logic;
	y35:		out	std_logic;
	y36:		out	std_logic;
	y37:		out	std_logic;
	y38:		out	std_logic;
	y39:		out	std_logic;
	y40:		out	std_logic;
	y41:		out	std_logic;
	y42:		out	std_logic;
	y43:		out	std_logic;
	y44:		out	std_logic;
	y45:		out	std_logic;
	y46:		out	std_logic;
	y47:		out	std_logic;
	y48:		out	std_logic;
	y49:		out	std_logic;
	y50:		out	std_logic;
	y51:		out	std_logic;
	y52:		out	std_logic;
	y53:		out	std_logic;
	y54:		out	std_logic;
	y55:		out	std_logic;
	y56:		out	std_logic;
	y57:		out	std_logic;
	y58:		out	std_logic;
	y59:		out	std_logic;
	y60:		out	std_logic;
	y61:		out	std_logic;
	y62:		out	std_logic;
	y63:		out	std_logic;
	y64:		out	std_logic;
	y65:		out	std_logic;
	y66:		out	std_logic;
	y67:		out	std_logic;
	y68:		out	std_logic;
	y69:		out	std_logic;
	y70:		out	std_logic;
	y71:		out	std_logic;
	y72:		out	std_logic;
	y73:		out	std_logic;
	y74:		out	std_logic;
	y75:		out	std_logic;
	y76:		out	std_logic;
	y77:		out	std_logic;
	y78:		out	std_logic;
	y79:		out	std_logic;
	y80:		out	std_logic;
	y81:		out	std_logic;
	y82:		out	std_logic;
	y83:		out	std_logic;
	y84:		out	std_logic;
	y85:		out	std_logic;
	y86:		out	std_logic;
	y87:		out	std_logic;
	y88:		out	std_logic;
	y89:		out	std_logic;
	y90:		out	std_logic;
	y91:		out	std_logic;
	y92:		out	std_logic;
	y93:		out	std_logic;
	y94:		out	std_logic;
	y95:		out	std_logic;
	y96:		out	std_logic;
	y97:		out	std_logic;
	y98:		out	std_logic;
	y99:		out	std_logic;
	y100:		out	std_logic;
	y101:		out	std_logic;
	y102:		out	std_logic;
	y103:		out	std_logic;
	y104:		out	std_logic;
	y105:		out	std_logic;
	y106:		out	std_logic;
	y107:		out	std_logic;
	y108:		out	std_logic;
	y109:		out	std_logic;
	y110:		out	std_logic;
	y111:		out	std_logic;
	y112:		out	std_logic;
	y113:		out	std_logic;
	y114:		out	std_logic;
	y115:		out	std_logic;
	y116:		out	std_logic;
	y117:		out	std_logic;
	y118:		out	std_logic;
	y119:		out	std_logic;
	y120:		out	std_logic;
	y121:		out	std_logic;
	y122:		out	std_logic;
	y123:		out	std_logic;
	y124:		out	std_logic;
	y125:		out	std_logic;
	y126:		out	std_logic;
	y127:		out	std_logic;
	address:	in	std_logic_vector(6 downto 0)
);

end component;

begin
	u1: for i in (N-1) downto 0 generate
		u: decoder1x128 port map(
			data	=> data(i),
			y0		=> y0(i),
			y1		=> y1(i),
			y2		=> y2(i),
			y3		=> y3(i),
			y4		=> y4(i),
			y5		=> y5(i),
			y6		=> y6(i),
			y7		=> y7(i),
			y8		=> y8(i),
			y9		=> y9(i),
			y10		=> y10(i),
			y11		=> y11(i),
			y12		=> y12(i),
			y13		=> y13(i),
			y14		=> y14(i),
			y15		=> y15(i),
			y16		=> y16(i),
			y17		=> y17(i),
			y18		=> y18(i),
			y19		=> y19(i),
			y20		=> y20(i),
			y21		=> y21(i),
			y22		=> y22(i),
			y23		=> y23(i),
			y24		=> y24(i),
			y25		=> y25(i),
			y26		=> y26(i),
			y27		=> y27(i),
			y28		=> y28(i),
			y29		=> y29(i),
			y30		=> y30(i),
			y31		=> y31(i),
			y32		=> y32(i),
			y33		=> y33(i),
			y34		=> y34(i),
			y35		=> y35(i),
			y36		=> y36(i),
			y37		=> y37(i),
			y38		=> y38(i),
			y39		=> y39(i),
			y40		=> y40(i),
			y41		=> y41(i),
			y42		=> y42(i),
			y43		=> y43(i),
			y44		=> y44(i),
			y45		=> y45(i),
			y46		=> y46(i),
			y47		=> y47(i),
			y48		=> y48(i),
			y49		=> y49(i),
			y50		=> y50(i),
			y51		=> y51(i),
			y52		=> y52(i),
			y53		=> y53(i),
			y54		=> y54(i),
			y55		=> y55(i),
			y56		=> y56(i),
			y57		=> y57(i),
			y58		=> y58(i),
			y59		=> y59(i),
			y60		=> y60(i),
			y61		=> y61(i),
			y62		=> y62(i),
			y63		=> y63(i),
			y64		=> y64(i),
			y65		=> y65(i),
			y66		=> y66(i),
			y67		=> y67(i),
			y68		=> y68(i),
			y69		=> y69(i),
			y70		=> y70(i),
			y71		=> y71(i),
			y72		=> y72(i),
			y73		=> y73(i),
			y74		=> y74(i),
			y75		=> y75(i),
			y76		=> y76(i),
			y77		=> y77(i),
			y78		=> y78(i),
			y79		=> y79(i),
			y80		=> y80(i),
			y81		=> y81(i),
			y82		=> y82(i),
			y83		=> y83(i),
			y84		=> y84(i),
			y85		=> y85(i),
			y86		=> y86(i),
			y87		=> y87(i),
			y88		=> y88(i),
			y89		=> y89(i),
			y90		=> y90(i),
			y91		=> y91(i),
			y92		=> y92(i),
			y93		=> y93(i),
			y94		=> y94(i),
			y95		=> y95(i),
			y96		=> y96(i),
			y97		=> y97(i),
			y98		=> y98(i),
			y99		=> y99(i),
			y100	=> y100(i),
			y101	=> y101(i),
			y102	=> y102(i),
			y103	=> y103(i),
			y104	=> y104(i),
			y105	=> y105(i),
			y106	=> y106(i),
			y107	=> y107(i),
			y108	=> y108(i),
			y109	=> y109(i),
			y110	=> y110(i),
			y111	=> y111(i),
			y112	=> y112(i),
			y113	=> y113(i),
			y114	=> y114(i),
			y115	=> y115(i),
			y116	=> y116(i),
			y117	=> y117(i),
			y118	=> y118(i),
			y119	=> y119(i),
			y120	=> y120(i),
			y121	=> y121(i),
			y122	=> y122(i),
			y123	=> y123(i),
			y124	=> y124(i),
			y125	=> y125(i),
			y126	=> y126(i),
			y127	=> y127(i),
			address	=> address
		);
	end generate u1;
end;

library	ieee;
use	ieee.std_logic_1164.all;

entity encoder256x1 is
port(
	data0:		in	std_logic;
	data1:		in	std_logic;
	data2:		in	std_logic;
	data3:		in	std_logic;
	data4:		in	std_logic;
	data5:		in	std_logic;
	data6:		in	std_logic;
	data7:		in	std_logic;
	data8:		in	std_logic;
	data9:		in	std_logic;
	data10:		in	std_logic;
	data11:		in	std_logic;
	data12:		in	std_logic;
	data13:		in	std_logic;
	data14:		in	std_logic;
	data15:		in	std_logic;
	data16:		in	std_logic;
	data17:		in	std_logic;
	data18:		in	std_logic;
	data19:		in	std_logic;
	data20:		in	std_logic;
	data21:		in	std_logic;
	data22:		in	std_logic;
	data23:		in	std_logic;
	data24:		in	std_logic;
	data25:		in	std_logic;
	data26:		in	std_logic;
	data27:		in	std_logic;
	data28:		in	std_logic;
	data29:		in	std_logic;
	data30:		in	std_logic;
	data31:		in	std_logic;
	data32:		in	std_logic;
	data33:		in	std_logic;
	data34:		in	std_logic;
	data35:		in	std_logic;
	data36:		in	std_logic;
	data37:		in	std_logic;
	data38:		in	std_logic;
	data39:		in	std_logic;
	data40:		in	std_logic;
	data41:		in	std_logic;
	data42:		in	std_logic;
	data43:		in	std_logic;
	data44:		in	std_logic;
	data45:		in	std_logic;
	data46:		in	std_logic;
	data47:		in	std_logic;
	data48:		in	std_logic;
	data49:		in	std_logic;
	data50:		in	std_logic;
	data51:		in	std_logic;
	data52:		in	std_logic;
	data53:		in	std_logic;
	data54:		in	std_logic;
	data55:		in	std_logic;
	data56:		in	std_logic;
	data57:		in	std_logic;
	data58:		in	std_logic;
	data59:		in	std_logic;
	data60:		in	std_logic;
	data61:		in	std_logic;
	data62:		in	std_logic;
	data63:		in	std_logic;
	data64:		in	std_logic;
	data65:		in	std_logic;
	data66:		in	std_logic;
	data67:		in	std_logic;
	data68:		in	std_logic;
	data69:		in	std_logic;
	data70:		in	std_logic;
	data71:		in	std_logic;
	data72:		in	std_logic;
	data73:		in	std_logic;
	data74:		in	std_logic;
	data75:		in	std_logic;
	data76:		in	std_logic;
	data77:		in	std_logic;
	data78:		in	std_logic;
	data79:		in	std_logic;
	data80:		in	std_logic;
	data81:		in	std_logic;
	data82:		in	std_logic;
	data83:		in	std_logic;
	data84:		in	std_logic;
	data85:		in	std_logic;
	data86:		in	std_logic;
	data87:		in	std_logic;
	data88:		in	std_logic;
	data89:		in	std_logic;
	data90:		in	std_logic;
	data91:		in	std_logic;
	data92:		in	std_logic;
	data93:		in	std_logic;
	data94:		in	std_logic;
	data95:		in	std_logic;
	data96:		in	std_logic;
	data97:		in	std_logic;
	data98:		in	std_logic;
	data99:		in	std_logic;
	data100:	in	std_logic;
	data101:	in	std_logic;
	data102:	in	std_logic;
	data103:	in	std_logic;
	data104:	in	std_logic;
	data105:	in	std_logic;
	data106:	in	std_logic;
	data107:	in	std_logic;
	data108:	in	std_logic;
	data109:	in	std_logic;
	data110:	in	std_logic;
	data111:	in	std_logic;
	data112:	in	std_logic;
	data113:	in	std_logic;
	data114:	in	std_logic;
	data115:	in	std_logic;
	data116:	in	std_logic;
	data117:	in	std_logic;
	data118:	in	std_logic;
	data119:	in	std_logic;
	data120:	in	std_logic;
	data121:	in	std_logic;
	data122:	in	std_logic;
	data123:	in	std_logic;
	data124:	in	std_logic;
	data125:	in	std_logic;
	data126:	in	std_logic;
	data127:	in	std_logic;
	data128:	in	std_logic;
	data129:	in	std_logic;
	data130:	in	std_logic;
	data131:	in	std_logic;
	data132:	in	std_logic;
	data133:	in	std_logic;
	data134:	in	std_logic;
	data135:	in	std_logic;
	data136:	in	std_logic;
	data137:	in	std_logic;
	data138:	in	std_logic;
	data139:	in	std_logic;
	data140:	in	std_logic;
	data141:	in	std_logic;
	data142:	in	std_logic;
	data143:	in	std_logic;
	data144:	in	std_logic;
	data145:	in	std_logic;
	data146:	in	std_logic;
	data147:	in	std_logic;
	data148:	in	std_logic;
	data149:	in	std_logic;
	data150:	in	std_logic;
	data151:	in	std_logic;
	data152:	in	std_logic;
	data153:	in	std_logic;
	data154:	in	std_logic;
	data155:	in	std_logic;
	data156:	in	std_logic;
	data157:	in	std_logic;
	data158:	in	std_logic;
	data159:	in	std_logic;
	data160:	in	std_logic;
	data161:	in	std_logic;
	data162:	in	std_logic;
	data163:	in	std_logic;
	data164:	in	std_logic;
	data165:	in	std_logic;
	data166:	in	std_logic;
	data167:	in	std_logic;
	data168:	in	std_logic;
	data169:	in	std_logic;
	data170:	in	std_logic;
	data171:	in	std_logic;
	data172:	in	std_logic;
	data173:	in	std_logic;
	data174:	in	std_logic;
	data175:	in	std_logic;
	data176:	in	std_logic;
	data177:	in	std_logic;
	data178:	in	std_logic;
	data179:	in	std_logic;
	data180:	in	std_logic;
	data181:	in	std_logic;
	data182:	in	std_logic;
	data183:	in	std_logic;
	data184:	in	std_logic;
	data185:	in	std_logic;
	data186:	in	std_logic;
	data187:	in	std_logic;
	data188:	in	std_logic;
	data189:	in	std_logic;
	data190:	in	std_logic;
	data191:	in	std_logic;
	data192:	in	std_logic;
	data193:	in	std_logic;
	data194:	in	std_logic;
	data195:	in	std_logic;
	data196:	in	std_logic;
	data197:	in	std_logic;
	data198:	in	std_logic;
	data199:	in	std_logic;
	data200:	in	std_logic;
	data201:	in	std_logic;
	data202:	in	std_logic;
	data203:	in	std_logic;
	data204:	in	std_logic;
	data205:	in	std_logic;
	data206:	in	std_logic;
	data207:	in	std_logic;
	data208:	in	std_logic;
	data209:	in	std_logic;
	data210:	in	std_logic;
	data211:	in	std_logic;
	data212:	in	std_logic;
	data213:	in	std_logic;
	data214:	in	std_logic;
	data215:	in	std_logic;
	data216:	in	std_logic;
	data217:	in	std_logic;
	data218:	in	std_logic;
	data219:	in	std_logic;
	data220:	in	std_logic;
	data221:	in	std_logic;
	data222:	in	std_logic;
	data223:	in	std_logic;
	data224:	in	std_logic;
	data225:	in	std_logic;
	data226:	in	std_logic;
	data227:	in	std_logic;
	data228:	in	std_logic;
	data229:	in	std_logic;
	data230:	in	std_logic;
	data231:	in	std_logic;
	data232:	in	std_logic;
	data233:	in	std_logic;
	data234:	in	std_logic;
	data235:	in	std_logic;
	data236:	in	std_logic;
	data237:	in	std_logic;
	data238:	in	std_logic;
	data239:	in	std_logic;
	data240:	in	std_logic;
	data241:	in	std_logic;
	data242:	in	std_logic;
	data243:	in	std_logic;
	data244:	in	std_logic;
	data245:	in	std_logic;
	data246:	in	std_logic;
	data247:	in	std_logic;
	data248:	in	std_logic;
	data249:	in	std_logic;
	data250:	in	std_logic;
	data251:	in	std_logic;
	data252:	in	std_logic;
	data253:	in	std_logic;
	data254:	in	std_logic;
	data255:	in	std_logic;
	address:	in	std_logic_vector(7 downto 0);
	output:		out	std_logic
);
end;

architecture struct_encoder256x1 of encoder256x1 is
begin
	with address select
		output <=
			data0	when "00000000",
			data1	when "00000001",
			data2	when "00000010",
			data3	when "00000011",
			data4	when "00000100",
			data5	when "00000101",
			data6	when "00000110",
			data7	when "00000111",
			data8	when "00001000",
			data9	when "00001001",
			data10	when "00001010",
			data11	when "00001011",
			data12	when "00001100",
			data13	when "00001101",
			data14	when "00001110",
			data15	when "00001111",
			data16	when "00010000",
			data17	when "00010001",
			data18	when "00010010",
			data19	when "00010011",
			data20	when "00010100",
			data21	when "00010101",
			data22	when "00010110",
			data23	when "00010111",
			data24	when "00011000",
			data25	when "00011001",
			data26	when "00011010",
			data27	when "00011011",
			data28	when "00011100",
			data29	when "00011101",
			data30	when "00011110",
			data31	when "00011111",
			data32	when "00100000",
			data33	when "00100001",
			data34	when "00100010",
			data35	when "00100011",
			data36	when "00100100",
			data37	when "00100101",
			data38	when "00100110",
			data39	when "00100111",
			data40	when "00101000",
			data41	when "00101001",
			data42	when "00101010",
			data43	when "00101011",
			data44	when "00101100",
			data45	when "00101101",
			data46	when "00101110",
			data47	when "00101111",
			data48	when "00110000",
			data49	when "00110001",
			data50	when "00110010",
			data51	when "00110011",
			data52	when "00110100",
			data53	when "00110101",
			data54	when "00110110",
			data55	when "00110111",
			data56	when "00111000",
			data57	when "00111001",
			data58	when "00111010",
			data59	when "00111011",
			data60	when "00111100",
			data61	when "00111101",
			data62	when "00111110",
			data63	when "00111111",
			data64	when "01000000",
			data65	when "01000001",
			data66	when "01000010",
			data67	when "01000011",
			data68	when "01000100",
			data69	when "01000101",
			data70	when "01000110",
			data71	when "01000111",
			data72	when "01001000",
			data73	when "01001001",
			data74	when "01001010",
			data75	when "01001011",
			data76	when "01001100",
			data77	when "01001101",
			data78	when "01001110",
			data79	when "01001111",
			data80	when "01010000",
			data81	when "01010001",
			data82	when "01010010",
			data83	when "01010011",
			data84	when "01010100",
			data85	when "01010101",
			data86	when "01010110",
			data87	when "01010111",
			data88	when "01011000",
			data89	when "01011001",
			data90	when "01011010",
			data91	when "01011011",
			data92	when "01011100",
			data93	when "01011101",
			data94	when "01011110",
			data95	when "01011111",
			data96	when "01100000",
			data97	when "01100001",
			data98	when "01100010",
			data99	when "01100011",
			data100	when "01100100",
			data101	when "01100101",
			data102	when "01100110",
			data103	when "01100111",
			data104	when "01101000",
			data105	when "01101001",
			data106	when "01101010",
			data107	when "01101011",
			data108	when "01101100",
			data109	when "01101101",
			data110	when "01101110",
			data111	when "01101111",
			data112	when "01110000",
			data113	when "01110001",
			data114	when "01110010",
			data115	when "01110011",
			data116	when "01110100",
			data117	when "01110101",
			data118	when "01110110",
			data119	when "01110111",
			data120	when "01111000",
			data121	when "01111001",
			data122	when "01111010",
			data123	when "01111011",
			data124	when "01111100",
			data125	when "01111101",
			data126	when "01111110",
			data127	when "01111111",
			data128	when "10000000",
			data129	when "10000001",
			data130	when "10000010",
			data131	when "10000011",
			data132	when "10000100",
			data133	when "10000101",
			data134	when "10000110",
			data135	when "10000111",
			data136	when "10001000",
			data137	when "10001001",
			data138	when "10001010",
			data139	when "10001011",
			data140	when "10001100",
			data141	when "10001101",
			data142	when "10001110",
			data143	when "10001111",
			data144	when "10010000",
			data145	when "10010001",
			data146	when "10010010",
			data147	when "10010011",
			data148	when "10010100",
			data149	when "10010101",
			data150	when "10010110",
			data151	when "10010111",
			data152	when "10011000",
			data153	when "10011001",
			data154	when "10011010",
			data155	when "10011011",
			data156	when "10011100",
			data157	when "10011101",
			data158	when "10011110",
			data159	when "10011111",
			data160	when "10100000",
			data161	when "10100001",
			data162	when "10100010",
			data163	when "10100011",
			data164	when "10100100",
			data165	when "10100101",
			data166	when "10100110",
			data167	when "10100111",
			data168	when "10101000",
			data169	when "10101001",
			data170	when "10101010",
			data171	when "10101011",
			data172	when "10101100",
			data173	when "10101101",
			data174	when "10101110",
			data175	when "10101111",
			data176	when "10110000",
			data177	when "10110001",
			data178	when "10110010",
			data179	when "10110011",
			data180	when "10110100",
			data181	when "10110101",
			data182	when "10110110",
			data183	when "10110111",
			data184	when "10111000",
			data185	when "10111001",
			data186	when "10111010",
			data187	when "10111011",
			data188	when "10111100",
			data189	when "10111101",
			data190	when "10111110",
			data191	when "10111111",
			data192	when "11000000",
			data193	when "11000001",
			data194	when "11000010",
			data195	when "11000011",
			data196	when "11000100",
			data197	when "11000101",
			data198	when "11000110",
			data199	when "11000111",
			data200	when "11001000",
			data201	when "11001001",
			data202	when "11001010",
			data203	when "11001011",
			data204	when "11001100",
			data205	when "11001101",
			data206	when "11001110",
			data207	when "11001111",
			data208	when "11010000",
			data209	when "11010001",
			data210	when "11010010",
			data211	when "11010011",
			data212	when "11010100",
			data213	when "11010101",
			data214	when "11010110",
			data215	when "11010111",
			data216	when "11011000",
			data217	when "11011001",
			data218	when "11011010",
			data219	when "11011011",
			data220	when "11011100",
			data221	when "11011101",
			data222	when "11011110",
			data223	when "11011111",
			data224	when "11100000",
			data225	when "11100001",
			data226	when "11100010",
			data227	when "11100011",
			data228	when "11100100",
			data229	when "11100101",
			data230	when "11100110",
			data231	when "11100111",
			data232	when "11101000",
			data233	when "11101001",
			data234	when "11101010",
			data235	when "11101011",
			data236	when "11101100",
			data237	when "11101101",
			data238	when "11101110",
			data239	when "11101111",
			data240	when "11110000",
			data241	when "11110001",
			data242	when "11110010",
			data243	when "11110011",
			data244	when "11110100",
			data245	when "11110101",
			data246	when "11110110",
			data247	when "11110111",
			data248	when "11111000",
			data249	when "11111001",
			data250	when "11111010",
			data251	when "11111011",
			data252	when "11111100",
			data253	when "11111101",
			data254	when "11111110",
			data255	when "11111111",
			'0' when others;
end;

library	ieee;
use	ieee.std_logic_1164.all;

entity encoder256xN is
generic(
	N:	positive
);
port(
	data0:		in	std_logic_vector((N-1) downto 0);
	data1:		in	std_logic_vector((N-1) downto 0);
	data2:		in	std_logic_vector((N-1) downto 0);
	data3:		in	std_logic_vector((N-1) downto 0);
	data4:		in	std_logic_vector((N-1) downto 0);
	data5:		in	std_logic_vector((N-1) downto 0);
	data6:		in	std_logic_vector((N-1) downto 0);
	data7:		in	std_logic_vector((N-1) downto 0);
	data8:		in	std_logic_vector((N-1) downto 0);
	data9:		in	std_logic_vector((N-1) downto 0);
	data10:		in	std_logic_vector((N-1) downto 0);
	data11:		in	std_logic_vector((N-1) downto 0);
	data12:		in	std_logic_vector((N-1) downto 0);
	data13:		in	std_logic_vector((N-1) downto 0);
	data14:		in	std_logic_vector((N-1) downto 0);
	data15:		in	std_logic_vector((N-1) downto 0);
	data16:		in	std_logic_vector((N-1) downto 0);
	data17:		in	std_logic_vector((N-1) downto 0);
	data18:		in	std_logic_vector((N-1) downto 0);
	data19:		in	std_logic_vector((N-1) downto 0);
	data20:		in	std_logic_vector((N-1) downto 0);
	data21:		in	std_logic_vector((N-1) downto 0);
	data22:		in	std_logic_vector((N-1) downto 0);
	data23:		in	std_logic_vector((N-1) downto 0);
	data24:		in	std_logic_vector((N-1) downto 0);
	data25:		in	std_logic_vector((N-1) downto 0);
	data26:		in	std_logic_vector((N-1) downto 0);
	data27:		in	std_logic_vector((N-1) downto 0);
	data28:		in	std_logic_vector((N-1) downto 0);
	data29:		in	std_logic_vector((N-1) downto 0);
	data30:		in	std_logic_vector((N-1) downto 0);
	data31:		in	std_logic_vector((N-1) downto 0);
	data32:		in	std_logic_vector((N-1) downto 0);
	data33:		in	std_logic_vector((N-1) downto 0);
	data34:		in	std_logic_vector((N-1) downto 0);
	data35:		in	std_logic_vector((N-1) downto 0);
	data36:		in	std_logic_vector((N-1) downto 0);
	data37:		in	std_logic_vector((N-1) downto 0);
	data38:		in	std_logic_vector((N-1) downto 0);
	data39:		in	std_logic_vector((N-1) downto 0);
	data40:		in	std_logic_vector((N-1) downto 0);
	data41:		in	std_logic_vector((N-1) downto 0);
	data42:		in	std_logic_vector((N-1) downto 0);
	data43:		in	std_logic_vector((N-1) downto 0);
	data44:		in	std_logic_vector((N-1) downto 0);
	data45:		in	std_logic_vector((N-1) downto 0);
	data46:		in	std_logic_vector((N-1) downto 0);
	data47:		in	std_logic_vector((N-1) downto 0);
	data48:		in	std_logic_vector((N-1) downto 0);
	data49:		in	std_logic_vector((N-1) downto 0);
	data50:		in	std_logic_vector((N-1) downto 0);
	data51:		in	std_logic_vector((N-1) downto 0);
	data52:		in	std_logic_vector((N-1) downto 0);
	data53:		in	std_logic_vector((N-1) downto 0);
	data54:		in	std_logic_vector((N-1) downto 0);
	data55:		in	std_logic_vector((N-1) downto 0);
	data56:		in	std_logic_vector((N-1) downto 0);
	data57:		in	std_logic_vector((N-1) downto 0);
	data58:		in	std_logic_vector((N-1) downto 0);
	data59:		in	std_logic_vector((N-1) downto 0);
	data60:		in	std_logic_vector((N-1) downto 0);
	data61:		in	std_logic_vector((N-1) downto 0);
	data62:		in	std_logic_vector((N-1) downto 0);
	data63:		in	std_logic_vector((N-1) downto 0);
	data64:		in	std_logic_vector((N-1) downto 0);
	data65:		in	std_logic_vector((N-1) downto 0);
	data66:		in	std_logic_vector((N-1) downto 0);
	data67:		in	std_logic_vector((N-1) downto 0);
	data68:		in	std_logic_vector((N-1) downto 0);
	data69:		in	std_logic_vector((N-1) downto 0);
	data70:		in	std_logic_vector((N-1) downto 0);
	data71:		in	std_logic_vector((N-1) downto 0);
	data72:		in	std_logic_vector((N-1) downto 0);
	data73:		in	std_logic_vector((N-1) downto 0);
	data74:		in	std_logic_vector((N-1) downto 0);
	data75:		in	std_logic_vector((N-1) downto 0);
	data76:		in	std_logic_vector((N-1) downto 0);
	data77:		in	std_logic_vector((N-1) downto 0);
	data78:		in	std_logic_vector((N-1) downto 0);
	data79:		in	std_logic_vector((N-1) downto 0);
	data80:		in	std_logic_vector((N-1) downto 0);
	data81:		in	std_logic_vector((N-1) downto 0);
	data82:		in	std_logic_vector((N-1) downto 0);
	data83:		in	std_logic_vector((N-1) downto 0);
	data84:		in	std_logic_vector((N-1) downto 0);
	data85:		in	std_logic_vector((N-1) downto 0);
	data86:		in	std_logic_vector((N-1) downto 0);
	data87:		in	std_logic_vector((N-1) downto 0);
	data88:		in	std_logic_vector((N-1) downto 0);
	data89:		in	std_logic_vector((N-1) downto 0);
	data90:		in	std_logic_vector((N-1) downto 0);
	data91:		in	std_logic_vector((N-1) downto 0);
	data92:		in	std_logic_vector((N-1) downto 0);
	data93:		in	std_logic_vector((N-1) downto 0);
	data94:		in	std_logic_vector((N-1) downto 0);
	data95:		in	std_logic_vector((N-1) downto 0);
	data96:		in	std_logic_vector((N-1) downto 0);
	data97:		in	std_logic_vector((N-1) downto 0);
	data98:		in	std_logic_vector((N-1) downto 0);
	data99:		in	std_logic_vector((N-1) downto 0);
	data100:	in	std_logic_vector((N-1) downto 0);
	data101:	in	std_logic_vector((N-1) downto 0);
	data102:	in	std_logic_vector((N-1) downto 0);
	data103:	in	std_logic_vector((N-1) downto 0);
	data104:	in	std_logic_vector((N-1) downto 0);
	data105:	in	std_logic_vector((N-1) downto 0);
	data106:	in	std_logic_vector((N-1) downto 0);
	data107:	in	std_logic_vector((N-1) downto 0);
	data108:	in	std_logic_vector((N-1) downto 0);
	data109:	in	std_logic_vector((N-1) downto 0);
	data110:	in	std_logic_vector((N-1) downto 0);
	data111:	in	std_logic_vector((N-1) downto 0);
	data112:	in	std_logic_vector((N-1) downto 0);
	data113:	in	std_logic_vector((N-1) downto 0);
	data114:	in	std_logic_vector((N-1) downto 0);
	data115:	in	std_logic_vector((N-1) downto 0);
	data116:	in	std_logic_vector((N-1) downto 0);
	data117:	in	std_logic_vector((N-1) downto 0);
	data118:	in	std_logic_vector((N-1) downto 0);
	data119:	in	std_logic_vector((N-1) downto 0);
	data120:	in	std_logic_vector((N-1) downto 0);
	data121:	in	std_logic_vector((N-1) downto 0);
	data122:	in	std_logic_vector((N-1) downto 0);
	data123:	in	std_logic_vector((N-1) downto 0);
	data124:	in	std_logic_vector((N-1) downto 0);
	data125:	in	std_logic_vector((N-1) downto 0);
	data126:	in	std_logic_vector((N-1) downto 0);
	data127:	in	std_logic_vector((N-1) downto 0);
	data128:	in	std_logic_vector((N-1) downto 0);
	data129:	in	std_logic_vector((N-1) downto 0);
	data130:	in	std_logic_vector((N-1) downto 0);
	data131:	in	std_logic_vector((N-1) downto 0);
	data132:	in	std_logic_vector((N-1) downto 0);
	data133:	in	std_logic_vector((N-1) downto 0);
	data134:	in	std_logic_vector((N-1) downto 0);
	data135:	in	std_logic_vector((N-1) downto 0);
	data136:	in	std_logic_vector((N-1) downto 0);
	data137:	in	std_logic_vector((N-1) downto 0);
	data138:	in	std_logic_vector((N-1) downto 0);
	data139:	in	std_logic_vector((N-1) downto 0);
	data140:	in	std_logic_vector((N-1) downto 0);
	data141:	in	std_logic_vector((N-1) downto 0);
	data142:	in	std_logic_vector((N-1) downto 0);
	data143:	in	std_logic_vector((N-1) downto 0);
	data144:	in	std_logic_vector((N-1) downto 0);
	data145:	in	std_logic_vector((N-1) downto 0);
	data146:	in	std_logic_vector((N-1) downto 0);
	data147:	in	std_logic_vector((N-1) downto 0);
	data148:	in	std_logic_vector((N-1) downto 0);
	data149:	in	std_logic_vector((N-1) downto 0);
	data150:	in	std_logic_vector((N-1) downto 0);
	data151:	in	std_logic_vector((N-1) downto 0);
	data152:	in	std_logic_vector((N-1) downto 0);
	data153:	in	std_logic_vector((N-1) downto 0);
	data154:	in	std_logic_vector((N-1) downto 0);
	data155:	in	std_logic_vector((N-1) downto 0);
	data156:	in	std_logic_vector((N-1) downto 0);
	data157:	in	std_logic_vector((N-1) downto 0);
	data158:	in	std_logic_vector((N-1) downto 0);
	data159:	in	std_logic_vector((N-1) downto 0);
	data160:	in	std_logic_vector((N-1) downto 0);
	data161:	in	std_logic_vector((N-1) downto 0);
	data162:	in	std_logic_vector((N-1) downto 0);
	data163:	in	std_logic_vector((N-1) downto 0);
	data164:	in	std_logic_vector((N-1) downto 0);
	data165:	in	std_logic_vector((N-1) downto 0);
	data166:	in	std_logic_vector((N-1) downto 0);
	data167:	in	std_logic_vector((N-1) downto 0);
	data168:	in	std_logic_vector((N-1) downto 0);
	data169:	in	std_logic_vector((N-1) downto 0);
	data170:	in	std_logic_vector((N-1) downto 0);
	data171:	in	std_logic_vector((N-1) downto 0);
	data172:	in	std_logic_vector((N-1) downto 0);
	data173:	in	std_logic_vector((N-1) downto 0);
	data174:	in	std_logic_vector((N-1) downto 0);
	data175:	in	std_logic_vector((N-1) downto 0);
	data176:	in	std_logic_vector((N-1) downto 0);
	data177:	in	std_logic_vector((N-1) downto 0);
	data178:	in	std_logic_vector((N-1) downto 0);
	data179:	in	std_logic_vector((N-1) downto 0);
	data180:	in	std_logic_vector((N-1) downto 0);
	data181:	in	std_logic_vector((N-1) downto 0);
	data182:	in	std_logic_vector((N-1) downto 0);
	data183:	in	std_logic_vector((N-1) downto 0);
	data184:	in	std_logic_vector((N-1) downto 0);
	data185:	in	std_logic_vector((N-1) downto 0);
	data186:	in	std_logic_vector((N-1) downto 0);
	data187:	in	std_logic_vector((N-1) downto 0);
	data188:	in	std_logic_vector((N-1) downto 0);
	data189:	in	std_logic_vector((N-1) downto 0);
	data190:	in	std_logic_vector((N-1) downto 0);
	data191:	in	std_logic_vector((N-1) downto 0);
	data192:	in	std_logic_vector((N-1) downto 0);
	data193:	in	std_logic_vector((N-1) downto 0);
	data194:	in	std_logic_vector((N-1) downto 0);
	data195:	in	std_logic_vector((N-1) downto 0);
	data196:	in	std_logic_vector((N-1) downto 0);
	data197:	in	std_logic_vector((N-1) downto 0);
	data198:	in	std_logic_vector((N-1) downto 0);
	data199:	in	std_logic_vector((N-1) downto 0);
	data200:	in	std_logic_vector((N-1) downto 0);
	data201:	in	std_logic_vector((N-1) downto 0);
	data202:	in	std_logic_vector((N-1) downto 0);
	data203:	in	std_logic_vector((N-1) downto 0);
	data204:	in	std_logic_vector((N-1) downto 0);
	data205:	in	std_logic_vector((N-1) downto 0);
	data206:	in	std_logic_vector((N-1) downto 0);
	data207:	in	std_logic_vector((N-1) downto 0);
	data208:	in	std_logic_vector((N-1) downto 0);
	data209:	in	std_logic_vector((N-1) downto 0);
	data210:	in	std_logic_vector((N-1) downto 0);
	data211:	in	std_logic_vector((N-1) downto 0);
	data212:	in	std_logic_vector((N-1) downto 0);
	data213:	in	std_logic_vector((N-1) downto 0);
	data214:	in	std_logic_vector((N-1) downto 0);
	data215:	in	std_logic_vector((N-1) downto 0);
	data216:	in	std_logic_vector((N-1) downto 0);
	data217:	in	std_logic_vector((N-1) downto 0);
	data218:	in	std_logic_vector((N-1) downto 0);
	data219:	in	std_logic_vector((N-1) downto 0);
	data220:	in	std_logic_vector((N-1) downto 0);
	data221:	in	std_logic_vector((N-1) downto 0);
	data222:	in	std_logic_vector((N-1) downto 0);
	data223:	in	std_logic_vector((N-1) downto 0);
	data224:	in	std_logic_vector((N-1) downto 0);
	data225:	in	std_logic_vector((N-1) downto 0);
	data226:	in	std_logic_vector((N-1) downto 0);
	data227:	in	std_logic_vector((N-1) downto 0);
	data228:	in	std_logic_vector((N-1) downto 0);
	data229:	in	std_logic_vector((N-1) downto 0);
	data230:	in	std_logic_vector((N-1) downto 0);
	data231:	in	std_logic_vector((N-1) downto 0);
	data232:	in	std_logic_vector((N-1) downto 0);
	data233:	in	std_logic_vector((N-1) downto 0);
	data234:	in	std_logic_vector((N-1) downto 0);
	data235:	in	std_logic_vector((N-1) downto 0);
	data236:	in	std_logic_vector((N-1) downto 0);
	data237:	in	std_logic_vector((N-1) downto 0);
	data238:	in	std_logic_vector((N-1) downto 0);
	data239:	in	std_logic_vector((N-1) downto 0);
	data240:	in	std_logic_vector((N-1) downto 0);
	data241:	in	std_logic_vector((N-1) downto 0);
	data242:	in	std_logic_vector((N-1) downto 0);
	data243:	in	std_logic_vector((N-1) downto 0);
	data244:	in	std_logic_vector((N-1) downto 0);
	data245:	in	std_logic_vector((N-1) downto 0);
	data246:	in	std_logic_vector((N-1) downto 0);
	data247:	in	std_logic_vector((N-1) downto 0);
	data248:	in	std_logic_vector((N-1) downto 0);
	data249:	in	std_logic_vector((N-1) downto 0);
	data250:	in	std_logic_vector((N-1) downto 0);
	data251:	in	std_logic_vector((N-1) downto 0);
	data252:	in	std_logic_vector((N-1) downto 0);
	data253:	in	std_logic_vector((N-1) downto 0);
	data254:	in	std_logic_vector((N-1) downto 0);
	data255:	in	std_logic_vector((N-1) downto 0);
	address:	in	std_logic_vector(7 downto 0);
	output:		out	std_logic_vector((N-1) downto 0)
);
end;

architecture struct_encoder256xN of encoder256xN is
component encoder256x1 is
port(
	data0:		in	std_logic;
	data1:		in	std_logic;
	data2:		in	std_logic;
	data3:		in	std_logic;
	data4:		in	std_logic;
	data5:		in	std_logic;
	data6:		in	std_logic;
	data7:		in	std_logic;
	data8:		in	std_logic;
	data9:		in	std_logic;
	data10:		in	std_logic;
	data11:		in	std_logic;
	data12:		in	std_logic;
	data13:		in	std_logic;
	data14:		in	std_logic;
	data15:		in	std_logic;
	data16:		in	std_logic;
	data17:		in	std_logic;
	data18:		in	std_logic;
	data19:		in	std_logic;
	data20:		in	std_logic;
	data21:		in	std_logic;
	data22:		in	std_logic;
	data23:		in	std_logic;
	data24:		in	std_logic;
	data25:		in	std_logic;
	data26:		in	std_logic;
	data27:		in	std_logic;
	data28:		in	std_logic;
	data29:		in	std_logic;
	data30:		in	std_logic;
	data31:		in	std_logic;
	data32:		in	std_logic;
	data33:		in	std_logic;
	data34:		in	std_logic;
	data35:		in	std_logic;
	data36:		in	std_logic;
	data37:		in	std_logic;
	data38:		in	std_logic;
	data39:		in	std_logic;
	data40:		in	std_logic;
	data41:		in	std_logic;
	data42:		in	std_logic;
	data43:		in	std_logic;
	data44:		in	std_logic;
	data45:		in	std_logic;
	data46:		in	std_logic;
	data47:		in	std_logic;
	data48:		in	std_logic;
	data49:		in	std_logic;
	data50:		in	std_logic;
	data51:		in	std_logic;
	data52:		in	std_logic;
	data53:		in	std_logic;
	data54:		in	std_logic;
	data55:		in	std_logic;
	data56:		in	std_logic;
	data57:		in	std_logic;
	data58:		in	std_logic;
	data59:		in	std_logic;
	data60:		in	std_logic;
	data61:		in	std_logic;
	data62:		in	std_logic;
	data63:		in	std_logic;
	data64:		in	std_logic;
	data65:		in	std_logic;
	data66:		in	std_logic;
	data67:		in	std_logic;
	data68:		in	std_logic;
	data69:		in	std_logic;
	data70:		in	std_logic;
	data71:		in	std_logic;
	data72:		in	std_logic;
	data73:		in	std_logic;
	data74:		in	std_logic;
	data75:		in	std_logic;
	data76:		in	std_logic;
	data77:		in	std_logic;
	data78:		in	std_logic;
	data79:		in	std_logic;
	data80:		in	std_logic;
	data81:		in	std_logic;
	data82:		in	std_logic;
	data83:		in	std_logic;
	data84:		in	std_logic;
	data85:		in	std_logic;
	data86:		in	std_logic;
	data87:		in	std_logic;
	data88:		in	std_logic;
	data89:		in	std_logic;
	data90:		in	std_logic;
	data91:		in	std_logic;
	data92:		in	std_logic;
	data93:		in	std_logic;
	data94:		in	std_logic;
	data95:		in	std_logic;
	data96:		in	std_logic;
	data97:		in	std_logic;
	data98:		in	std_logic;
	data99:		in	std_logic;
	data100:	in	std_logic;
	data101:	in	std_logic;
	data102:	in	std_logic;
	data103:	in	std_logic;
	data104:	in	std_logic;
	data105:	in	std_logic;
	data106:	in	std_logic;
	data107:	in	std_logic;
	data108:	in	std_logic;
	data109:	in	std_logic;
	data110:	in	std_logic;
	data111:	in	std_logic;
	data112:	in	std_logic;
	data113:	in	std_logic;
	data114:	in	std_logic;
	data115:	in	std_logic;
	data116:	in	std_logic;
	data117:	in	std_logic;
	data118:	in	std_logic;
	data119:	in	std_logic;
	data120:	in	std_logic;
	data121:	in	std_logic;
	data122:	in	std_logic;
	data123:	in	std_logic;
	data124:	in	std_logic;
	data125:	in	std_logic;
	data126:	in	std_logic;
	data127:	in	std_logic;
	data128:	in	std_logic;
	data129:	in	std_logic;
	data130:	in	std_logic;
	data131:	in	std_logic;
	data132:	in	std_logic;
	data133:	in	std_logic;
	data134:	in	std_logic;
	data135:	in	std_logic;
	data136:	in	std_logic;
	data137:	in	std_logic;
	data138:	in	std_logic;
	data139:	in	std_logic;
	data140:	in	std_logic;
	data141:	in	std_logic;
	data142:	in	std_logic;
	data143:	in	std_logic;
	data144:	in	std_logic;
	data145:	in	std_logic;
	data146:	in	std_logic;
	data147:	in	std_logic;
	data148:	in	std_logic;
	data149:	in	std_logic;
	data150:	in	std_logic;
	data151:	in	std_logic;
	data152:	in	std_logic;
	data153:	in	std_logic;
	data154:	in	std_logic;
	data155:	in	std_logic;
	data156:	in	std_logic;
	data157:	in	std_logic;
	data158:	in	std_logic;
	data159:	in	std_logic;
	data160:	in	std_logic;
	data161:	in	std_logic;
	data162:	in	std_logic;
	data163:	in	std_logic;
	data164:	in	std_logic;
	data165:	in	std_logic;
	data166:	in	std_logic;
	data167:	in	std_logic;
	data168:	in	std_logic;
	data169:	in	std_logic;
	data170:	in	std_logic;
	data171:	in	std_logic;
	data172:	in	std_logic;
	data173:	in	std_logic;
	data174:	in	std_logic;
	data175:	in	std_logic;
	data176:	in	std_logic;
	data177:	in	std_logic;
	data178:	in	std_logic;
	data179:	in	std_logic;
	data180:	in	std_logic;
	data181:	in	std_logic;
	data182:	in	std_logic;
	data183:	in	std_logic;
	data184:	in	std_logic;
	data185:	in	std_logic;
	data186:	in	std_logic;
	data187:	in	std_logic;
	data188:	in	std_logic;
	data189:	in	std_logic;
	data190:	in	std_logic;
	data191:	in	std_logic;
	data192:	in	std_logic;
	data193:	in	std_logic;
	data194:	in	std_logic;
	data195:	in	std_logic;
	data196:	in	std_logic;
	data197:	in	std_logic;
	data198:	in	std_logic;
	data199:	in	std_logic;
	data200:	in	std_logic;
	data201:	in	std_logic;
	data202:	in	std_logic;
	data203:	in	std_logic;
	data204:	in	std_logic;
	data205:	in	std_logic;
	data206:	in	std_logic;
	data207:	in	std_logic;
	data208:	in	std_logic;
	data209:	in	std_logic;
	data210:	in	std_logic;
	data211:	in	std_logic;
	data212:	in	std_logic;
	data213:	in	std_logic;
	data214:	in	std_logic;
	data215:	in	std_logic;
	data216:	in	std_logic;
	data217:	in	std_logic;
	data218:	in	std_logic;
	data219:	in	std_logic;
	data220:	in	std_logic;
	data221:	in	std_logic;
	data222:	in	std_logic;
	data223:	in	std_logic;
	data224:	in	std_logic;
	data225:	in	std_logic;
	data226:	in	std_logic;
	data227:	in	std_logic;
	data228:	in	std_logic;
	data229:	in	std_logic;
	data230:	in	std_logic;
	data231:	in	std_logic;
	data232:	in	std_logic;
	data233:	in	std_logic;
	data234:	in	std_logic;
	data235:	in	std_logic;
	data236:	in	std_logic;
	data237:	in	std_logic;
	data238:	in	std_logic;
	data239:	in	std_logic;
	data240:	in	std_logic;
	data241:	in	std_logic;
	data242:	in	std_logic;
	data243:	in	std_logic;
	data244:	in	std_logic;
	data245:	in	std_logic;
	data246:	in	std_logic;
	data247:	in	std_logic;
	data248:	in	std_logic;
	data249:	in	std_logic;
	data250:	in	std_logic;
	data251:	in	std_logic;
	data252:	in	std_logic;
	data253:	in	std_logic;
	data254:	in	std_logic;
	data255:	in	std_logic;
	address:	in	std_logic_vector(7 downto 0);
	output:		out	std_logic
);
end component;

begin
	u1: for i in (N-1) downto 0 generate
		u: encoder256x1 port map(
			data0		=> data0(i),
			data1		=> data1(i),
			data2		=> data2(i),
			data3		=> data3(i),
			data4		=> data4(i),
			data5		=> data5(i),
			data6		=> data6(i),
			data7		=> data7(i),
			data8		=> data8(i),
			data9		=> data9(i),
			data10		=> data10(i),
			data11		=> data11(i),
			data12		=> data12(i),
			data13		=> data13(i),
			data14		=> data14(i),
			data15		=> data15(i),
			data16		=> data16(i),
			data17		=> data17(i),
			data18		=> data18(i),
			data19		=> data19(i),
			data20		=> data20(i),
			data21		=> data21(i),
			data22		=> data22(i),
			data23		=> data23(i),
			data24		=> data24(i),
			data25		=> data25(i),
			data26		=> data26(i),
			data27		=> data27(i),
			data28		=> data28(i),
			data29		=> data29(i),
			data30		=> data30(i),
			data31		=> data31(i),
			data32		=> data32(i),
			data33		=> data33(i),
			data34		=> data34(i),
			data35		=> data35(i),
			data36		=> data36(i),
			data37		=> data37(i),
			data38		=> data38(i),
			data39		=> data39(i),
			data40		=> data40(i),
			data41		=> data41(i),
			data42		=> data42(i),
			data43		=> data43(i),
			data44		=> data44(i),
			data45		=> data45(i),
			data46		=> data46(i),
			data47		=> data47(i),
			data48		=> data48(i),
			data49		=> data49(i),
			data50		=> data50(i),
			data51		=> data51(i),
			data52		=> data52(i),
			data53		=> data53(i),
			data54		=> data54(i),
			data55		=> data55(i),
			data56		=> data56(i),
			data57		=> data57(i),
			data58		=> data58(i),
			data59		=> data59(i),
			data60		=> data60(i),
			data61		=> data61(i),
			data62		=> data62(i),
			data63		=> data63(i),
			data64		=> data64(i),
			data65		=> data65(i),
			data66		=> data66(i),
			data67		=> data67(i),
			data68		=> data68(i),
			data69		=> data69(i),
			data70		=> data70(i),
			data71		=> data71(i),
			data72		=> data72(i),
			data73		=> data73(i),
			data74		=> data74(i),
			data75		=> data75(i),
			data76		=> data76(i),
			data77		=> data77(i),
			data78		=> data78(i),
			data79		=> data79(i),
			data80		=> data80(i),
			data81		=> data81(i),
			data82		=> data82(i),
			data83		=> data83(i),
			data84		=> data84(i),
			data85		=> data85(i),
			data86		=> data86(i),
			data87		=> data87(i),
			data88		=> data88(i),
			data89		=> data89(i),
			data90		=> data90(i),
			data91		=> data91(i),
			data92		=> data92(i),
			data93		=> data93(i),
			data94		=> data94(i),
			data95		=> data95(i),
			data96		=> data96(i),
			data97		=> data97(i),
			data98		=> data98(i),
			data99		=> data99(i),
			data100		=> data100(i),
			data101		=> data101(i),
			data102		=> data102(i),
			data103		=> data103(i),
			data104		=> data104(i),
			data105		=> data105(i),
			data106		=> data106(i),
			data107		=> data107(i),
			data108		=> data108(i),
			data109		=> data109(i),
			data110		=> data110(i),
			data111		=> data111(i),
			data112		=> data112(i),
			data113		=> data113(i),
			data114		=> data114(i),
			data115		=> data115(i),
			data116		=> data116(i),
			data117		=> data117(i),
			data118		=> data118(i),
			data119		=> data119(i),
			data120		=> data120(i),
			data121		=> data121(i),
			data122		=> data122(i),
			data123		=> data123(i),
			data124		=> data124(i),
			data125		=> data125(i),
			data126		=> data126(i),
			data127		=> data127(i),
			data128		=> data128(i),
			data129		=> data129(i),
			data130		=> data130(i),
			data131		=> data131(i),
			data132		=> data132(i),
			data133		=> data133(i),
			data134		=> data134(i),
			data135		=> data135(i),
			data136		=> data136(i),
			data137		=> data137(i),
			data138		=> data138(i),
			data139		=> data139(i),
			data140		=> data140(i),
			data141		=> data141(i),
			data142		=> data142(i),
			data143		=> data143(i),
			data144		=> data144(i),
			data145		=> data145(i),
			data146		=> data146(i),
			data147		=> data147(i),
			data148		=> data148(i),
			data149		=> data149(i),
			data150		=> data150(i),
			data151		=> data151(i),
			data152		=> data152(i),
			data153		=> data153(i),
			data154		=> data154(i),
			data155		=> data155(i),
			data156		=> data156(i),
			data157		=> data157(i),
			data158		=> data158(i),
			data159		=> data159(i),
			data160		=> data160(i),
			data161		=> data161(i),
			data162		=> data162(i),
			data163		=> data163(i),
			data164		=> data164(i),
			data165		=> data165(i),
			data166		=> data166(i),
			data167		=> data167(i),
			data168		=> data168(i),
			data169		=> data169(i),
			data170		=> data170(i),
			data171		=> data171(i),
			data172		=> data172(i),
			data173		=> data173(i),
			data174		=> data174(i),
			data175		=> data175(i),
			data176		=> data176(i),
			data177		=> data177(i),
			data178		=> data178(i),
			data179		=> data179(i),
			data180		=> data180(i),
			data181		=> data181(i),
			data182		=> data182(i),
			data183		=> data183(i),
			data184		=> data184(i),
			data185		=> data185(i),
			data186		=> data186(i),
			data187		=> data187(i),
			data188		=> data188(i),
			data189		=> data189(i),
			data190		=> data190(i),
			data191		=> data191(i),
			data192		=> data192(i),
			data193		=> data193(i),
			data194		=> data194(i),
			data195		=> data195(i),
			data196		=> data196(i),
			data197		=> data197(i),
			data198		=> data198(i),
			data199		=> data199(i),
			data200		=> data200(i),
			data201		=> data201(i),
			data202		=> data202(i),
			data203		=> data203(i),
			data204		=> data204(i),
			data205		=> data205(i),
			data206		=> data206(i),
			data207		=> data207(i),
			data208		=> data208(i),
			data209		=> data209(i),
			data210		=> data210(i),
			data211		=> data211(i),
			data212		=> data212(i),
			data213		=> data213(i),
			data214		=> data214(i),
			data215		=> data215(i),
			data216		=> data216(i),
			data217		=> data217(i),
			data218		=> data218(i),
			data219		=> data219(i),
			data220		=> data220(i),
			data221		=> data221(i),
			data222		=> data222(i),
			data223		=> data223(i),
			data224		=> data224(i),
			data225		=> data225(i),
			data226		=> data226(i),
			data227		=> data227(i),
			data228		=> data228(i),
			data229		=> data229(i),
			data230		=> data230(i),
			data231		=> data231(i),
			data232		=> data232(i),
			data233		=> data233(i),
			data234		=> data234(i),
			data235		=> data235(i),
			data236		=> data236(i),
			data237		=> data237(i),
			data238		=> data238(i),
			data239		=> data239(i),
			data240		=> data240(i),
			data241		=> data241(i),
			data242		=> data242(i),
			data243		=> data243(i),
			data244		=> data244(i),
			data245		=> data245(i),
			data246		=> data246(i),
			data247		=> data247(i),
			data248		=> data248(i),
			data249		=> data249(i),
			data250		=> data250(i),
			data251		=> data251(i),
			data252		=> data252(i),
			data253		=> data253(i),
			data254		=> data254(i),
			data255		=> data255(i),
			address		=> address,
			output		=> output(i)
		);
	end generate u1;
end;

library	ieee;
use	ieee.std_logic_1164.all;

entity decoder1x256 is
port(
	data:		in	std_logic;
	y0:			out	std_logic;
	y1:			out	std_logic;
	y2:			out	std_logic;
	y3:			out	std_logic;
	y4:			out	std_logic;
	y5:			out	std_logic;
	y6:			out	std_logic;
	y7:			out	std_logic;
	y8:			out	std_logic;
	y9:			out	std_logic;
	y10:		out	std_logic;
	y11:		out	std_logic;
	y12:		out	std_logic;
	y13:		out	std_logic;
	y14:		out	std_logic;
	y15:		out	std_logic;
	y16:		out	std_logic;
	y17:		out	std_logic;
	y18:		out	std_logic;
	y19:		out	std_logic;
	y20:		out	std_logic;
	y21:		out	std_logic;
	y22:		out	std_logic;
	y23:		out	std_logic;
	y24:		out	std_logic;
	y25:		out	std_logic;
	y26:		out	std_logic;
	y27:		out	std_logic;
	y28:		out	std_logic;
	y29:		out	std_logic;
	y30:		out	std_logic;
	y31:		out	std_logic;
	y32:		out	std_logic;
	y33:		out	std_logic;
	y34:		out	std_logic;
	y35:		out	std_logic;
	y36:		out	std_logic;
	y37:		out	std_logic;
	y38:		out	std_logic;
	y39:		out	std_logic;
	y40:		out	std_logic;
	y41:		out	std_logic;
	y42:		out	std_logic;
	y43:		out	std_logic;
	y44:		out	std_logic;
	y45:		out	std_logic;
	y46:		out	std_logic;
	y47:		out	std_logic;
	y48:		out	std_logic;
	y49:		out	std_logic;
	y50:		out	std_logic;
	y51:		out	std_logic;
	y52:		out	std_logic;
	y53:		out	std_logic;
	y54:		out	std_logic;
	y55:		out	std_logic;
	y56:		out	std_logic;
	y57:		out	std_logic;
	y58:		out	std_logic;
	y59:		out	std_logic;
	y60:		out	std_logic;
	y61:		out	std_logic;
	y62:		out	std_logic;
	y63:		out	std_logic;
	y64:		out	std_logic;
	y65:		out	std_logic;
	y66:		out	std_logic;
	y67:		out	std_logic;
	y68:		out	std_logic;
	y69:		out	std_logic;
	y70:		out	std_logic;
	y71:		out	std_logic;
	y72:		out	std_logic;
	y73:		out	std_logic;
	y74:		out	std_logic;
	y75:		out	std_logic;
	y76:		out	std_logic;
	y77:		out	std_logic;
	y78:		out	std_logic;
	y79:		out	std_logic;
	y80:		out	std_logic;
	y81:		out	std_logic;
	y82:		out	std_logic;
	y83:		out	std_logic;
	y84:		out	std_logic;
	y85:		out	std_logic;
	y86:		out	std_logic;
	y87:		out	std_logic;
	y88:		out	std_logic;
	y89:		out	std_logic;
	y90:		out	std_logic;
	y91:		out	std_logic;
	y92:		out	std_logic;
	y93:		out	std_logic;
	y94:		out	std_logic;
	y95:		out	std_logic;
	y96:		out	std_logic;
	y97:		out	std_logic;
	y98:		out	std_logic;
	y99:		out	std_logic;
	y100:		out	std_logic;
	y101:		out	std_logic;
	y102:		out	std_logic;
	y103:		out	std_logic;
	y104:		out	std_logic;
	y105:		out	std_logic;
	y106:		out	std_logic;
	y107:		out	std_logic;
	y108:		out	std_logic;
	y109:		out	std_logic;
	y110:		out	std_logic;
	y111:		out	std_logic;
	y112:		out	std_logic;
	y113:		out	std_logic;
	y114:		out	std_logic;
	y115:		out	std_logic;
	y116:		out	std_logic;
	y117:		out	std_logic;
	y118:		out	std_logic;
	y119:		out	std_logic;
	y120:		out	std_logic;
	y121:		out	std_logic;
	y122:		out	std_logic;
	y123:		out	std_logic;
	y124:		out	std_logic;
	y125:		out	std_logic;
	y126:		out	std_logic;
	y127:		out	std_logic;
	y128:		out	std_logic;
	y129:		out	std_logic;
	y130:		out	std_logic;
	y131:		out	std_logic;
	y132:		out	std_logic;
	y133:		out	std_logic;
	y134:		out	std_logic;
	y135:		out	std_logic;
	y136:		out	std_logic;
	y137:		out	std_logic;
	y138:		out	std_logic;
	y139:		out	std_logic;
	y140:		out	std_logic;
	y141:		out	std_logic;
	y142:		out	std_logic;
	y143:		out	std_logic;
	y144:		out	std_logic;
	y145:		out	std_logic;
	y146:		out	std_logic;
	y147:		out	std_logic;
	y148:		out	std_logic;
	y149:		out	std_logic;
	y150:		out	std_logic;
	y151:		out	std_logic;
	y152:		out	std_logic;
	y153:		out	std_logic;
	y154:		out	std_logic;
	y155:		out	std_logic;
	y156:		out	std_logic;
	y157:		out	std_logic;
	y158:		out	std_logic;
	y159:		out	std_logic;
	y160:		out	std_logic;
	y161:		out	std_logic;
	y162:		out	std_logic;
	y163:		out	std_logic;
	y164:		out	std_logic;
	y165:		out	std_logic;
	y166:		out	std_logic;
	y167:		out	std_logic;
	y168:		out	std_logic;
	y169:		out	std_logic;
	y170:		out	std_logic;
	y171:		out	std_logic;
	y172:		out	std_logic;
	y173:		out	std_logic;
	y174:		out	std_logic;
	y175:		out	std_logic;
	y176:		out	std_logic;
	y177:		out	std_logic;
	y178:		out	std_logic;
	y179:		out	std_logic;
	y180:		out	std_logic;
	y181:		out	std_logic;
	y182:		out	std_logic;
	y183:		out	std_logic;
	y184:		out	std_logic;
	y185:		out	std_logic;
	y186:		out	std_logic;
	y187:		out	std_logic;
	y188:		out	std_logic;
	y189:		out	std_logic;
	y190:		out	std_logic;
	y191:		out	std_logic;
	y192:		out	std_logic;
	y193:		out	std_logic;
	y194:		out	std_logic;
	y195:		out	std_logic;
	y196:		out	std_logic;
	y197:		out	std_logic;
	y198:		out	std_logic;
	y199:		out	std_logic;
	y200:		out	std_logic;
	y201:		out	std_logic;
	y202:		out	std_logic;
	y203:		out	std_logic;
	y204:		out	std_logic;
	y205:		out	std_logic;
	y206:		out	std_logic;
	y207:		out	std_logic;
	y208:		out	std_logic;
	y209:		out	std_logic;
	y210:		out	std_logic;
	y211:		out	std_logic;
	y212:		out	std_logic;
	y213:		out	std_logic;
	y214:		out	std_logic;
	y215:		out	std_logic;
	y216:		out	std_logic;
	y217:		out	std_logic;
	y218:		out	std_logic;
	y219:		out	std_logic;
	y220:		out	std_logic;
	y221:		out	std_logic;
	y222:		out	std_logic;
	y223:		out	std_logic;
	y224:		out	std_logic;
	y225:		out	std_logic;
	y226:		out	std_logic;
	y227:		out	std_logic;
	y228:		out	std_logic;
	y229:		out	std_logic;
	y230:		out	std_logic;
	y231:		out	std_logic;
	y232:		out	std_logic;
	y233:		out	std_logic;
	y234:		out	std_logic;
	y235:		out	std_logic;
	y236:		out	std_logic;
	y237:		out	std_logic;
	y238:		out	std_logic;
	y239:		out	std_logic;
	y240:		out	std_logic;
	y241:		out	std_logic;
	y242:		out	std_logic;
	y243:		out	std_logic;
	y244:		out	std_logic;
	y245:		out	std_logic;
	y246:		out	std_logic;
	y247:		out	std_logic;
	y248:		out	std_logic;
	y249:		out	std_logic;
	y250:		out	std_logic;
	y251:		out	std_logic;
	y252:		out	std_logic;
	y253:		out	std_logic;
	y254:		out	std_logic;
	y255:		out	std_logic;
	address:	in	std_logic_vector(7 downto 0)
);
end;

architecture struct_decoder1x256 of decoder1x256 is
begin
	y0		<= data when address = "00000000" else '0';
	y1		<= data when address = "00000001" else '0';
	y2		<= data when address = "00000010" else '0';
	y3		<= data when address = "00000011" else '0';
	y4		<= data when address = "00000100" else '0';
	y5		<= data when address = "00000101" else '0';
	y6		<= data when address = "00000110" else '0';
	y7		<= data when address = "00000111" else '0';
	y8		<= data when address = "00001000" else '0';
	y9		<= data when address = "00001001" else '0';
	y10		<= data when address = "00001010" else '0';
	y11		<= data when address = "00001011" else '0';
	y12		<= data when address = "00001100" else '0';
	y13		<= data when address = "00001101" else '0';
	y14		<= data when address = "00001110" else '0';
	y15		<= data when address = "00001111" else '0';
	y16		<= data when address = "00010000" else '0';
	y17		<= data when address = "00010001" else '0';
	y18		<= data when address = "00010010" else '0';
	y19		<= data when address = "00010011" else '0';
	y20		<= data when address = "00010100" else '0';
	y21		<= data when address = "00010101" else '0';
	y22		<= data when address = "00010110" else '0';
	y23		<= data when address = "00010111" else '0';
	y24		<= data when address = "00011000" else '0';
	y25		<= data when address = "00011001" else '0';
	y26		<= data when address = "00011010" else '0';
	y27		<= data when address = "00011011" else '0';
	y28		<= data when address = "00011100" else '0';
	y29		<= data when address = "00011101" else '0';
	y30		<= data when address = "00011110" else '0';
	y31		<= data when address = "00011111" else '0';
	y32		<= data when address = "00100000" else '0';
	y33		<= data when address = "00100001" else '0';
	y34		<= data when address = "00100010" else '0';
	y35		<= data when address = "00100011" else '0';
	y36		<= data when address = "00100100" else '0';
	y37		<= data when address = "00100101" else '0';
	y38		<= data when address = "00100110" else '0';
	y39		<= data when address = "00100111" else '0';
	y40		<= data when address = "00101000" else '0';
	y41		<= data when address = "00101001" else '0';
	y42		<= data when address = "00101010" else '0';
	y43		<= data when address = "00101011" else '0';
	y44		<= data when address = "00101100" else '0';
	y45		<= data when address = "00101101" else '0';
	y46		<= data when address = "00101110" else '0';
	y47		<= data when address = "00101111" else '0';
	y48		<= data when address = "00110000" else '0';
	y49		<= data when address = "00110001" else '0';
	y50		<= data when address = "00110010" else '0';
	y51		<= data when address = "00110011" else '0';
	y52		<= data when address = "00110100" else '0';
	y53		<= data when address = "00110101" else '0';
	y54		<= data when address = "00110110" else '0';
	y55		<= data when address = "00110111" else '0';
	y56		<= data when address = "00111000" else '0';
	y57		<= data when address = "00111001" else '0';
	y58		<= data when address = "00111010" else '0';
	y59		<= data when address = "00111011" else '0';
	y60		<= data when address = "00111100" else '0';
	y61		<= data when address = "00111101" else '0';
	y62		<= data when address = "00111110" else '0';
	y63		<= data when address = "00111111" else '0';
	y64		<= data when address = "01000000" else '0';
	y65		<= data when address = "01000001" else '0';
	y66		<= data when address = "01000010" else '0';
	y67		<= data when address = "01000011" else '0';
	y68		<= data when address = "01000100" else '0';
	y69		<= data when address = "01000101" else '0';
	y70		<= data when address = "01000110" else '0';
	y71		<= data when address = "01000111" else '0';
	y72		<= data when address = "01001000" else '0';
	y73		<= data when address = "01001001" else '0';
	y74		<= data when address = "01001010" else '0';
	y75		<= data when address = "01001011" else '0';
	y76		<= data when address = "01001100" else '0';
	y77		<= data when address = "01001101" else '0';
	y78		<= data when address = "01001110" else '0';
	y79		<= data when address = "01001111" else '0';
	y80		<= data when address = "01010000" else '0';
	y81		<= data when address = "01010001" else '0';
	y82		<= data when address = "01010010" else '0';
	y83		<= data when address = "01010011" else '0';
	y84		<= data when address = "01010100" else '0';
	y85		<= data when address = "01010101" else '0';
	y86		<= data when address = "01010110" else '0';
	y87		<= data when address = "01010111" else '0';
	y88		<= data when address = "01011000" else '0';
	y89		<= data when address = "01011001" else '0';
	y90		<= data when address = "01011010" else '0';
	y91		<= data when address = "01011011" else '0';
	y92		<= data when address = "01011100" else '0';
	y93		<= data when address = "01011101" else '0';
	y94		<= data when address = "01011110" else '0';
	y95		<= data when address = "01011111" else '0';
	y96		<= data when address = "01100000" else '0';
	y97		<= data when address = "01100001" else '0';
	y98		<= data when address = "01100010" else '0';
	y99		<= data when address = "01100011" else '0';
	y100	<= data when address = "01100100" else '0';
	y101	<= data when address = "01100101" else '0';
	y102	<= data when address = "01100110" else '0';
	y103	<= data when address = "01100111" else '0';
	y104	<= data when address = "01101000" else '0';
	y105	<= data when address = "01101001" else '0';
	y106	<= data when address = "01101010" else '0';
	y107	<= data when address = "01101011" else '0';
	y108	<= data when address = "01101100" else '0';
	y109	<= data when address = "01101101" else '0';
	y110	<= data when address = "01101110" else '0';
	y111	<= data when address = "01101111" else '0';
	y112	<= data when address = "01110000" else '0';
	y113	<= data when address = "01110001" else '0';
	y114	<= data when address = "01110010" else '0';
	y115	<= data when address = "01110011" else '0';
	y116	<= data when address = "01110100" else '0';
	y117	<= data when address = "01110101" else '0';
	y118	<= data when address = "01110110" else '0';
	y119	<= data when address = "01110111" else '0';
	y120	<= data when address = "01111000" else '0';
	y121	<= data when address = "01111001" else '0';
	y122	<= data when address = "01111010" else '0';
	y123	<= data when address = "01111011" else '0';
	y124	<= data when address = "01111100" else '0';
	y125	<= data when address = "01111101" else '0';
	y126	<= data when address = "01111110" else '0';
	y127	<= data when address = "01111111" else '0';
	y128	<= data when address = "10000000" else '0';
	y129	<= data when address = "10000001" else '0';
	y130	<= data when address = "10000010" else '0';
	y131	<= data when address = "10000011" else '0';
	y132	<= data when address = "10000100" else '0';
	y133	<= data when address = "10000101" else '0';
	y134	<= data when address = "10000110" else '0';
	y135	<= data when address = "10000111" else '0';
	y136	<= data when address = "10001000" else '0';
	y137	<= data when address = "10001001" else '0';
	y138	<= data when address = "10001010" else '0';
	y139	<= data when address = "10001011" else '0';
	y140	<= data when address = "10001100" else '0';
	y141	<= data when address = "10001101" else '0';
	y142	<= data when address = "10001110" else '0';
	y143	<= data when address = "10001111" else '0';
	y144	<= data when address = "10010000" else '0';
	y145	<= data when address = "10010001" else '0';
	y146	<= data when address = "10010010" else '0';
	y147	<= data when address = "10010011" else '0';
	y148	<= data when address = "10010100" else '0';
	y149	<= data when address = "10010101" else '0';
	y150	<= data when address = "10010110" else '0';
	y151	<= data when address = "10010111" else '0';
	y152	<= data when address = "10011000" else '0';
	y153	<= data when address = "10011001" else '0';
	y154	<= data when address = "10011010" else '0';
	y155	<= data when address = "10011011" else '0';
	y156	<= data when address = "10011100" else '0';
	y157	<= data when address = "10011101" else '0';
	y158	<= data when address = "10011110" else '0';
	y159	<= data when address = "10011111" else '0';
	y160	<= data when address = "10100000" else '0';
	y161	<= data when address = "10100001" else '0';
	y162	<= data when address = "10100010" else '0';
	y163	<= data when address = "10100011" else '0';
	y164	<= data when address = "10100100" else '0';
	y165	<= data when address = "10100101" else '0';
	y166	<= data when address = "10100110" else '0';
	y167	<= data when address = "10100111" else '0';
	y168	<= data when address = "10101000" else '0';
	y169	<= data when address = "10101001" else '0';
	y170	<= data when address = "10101010" else '0';
	y171	<= data when address = "10101011" else '0';
	y172	<= data when address = "10101100" else '0';
	y173	<= data when address = "10101101" else '0';
	y174	<= data when address = "10101110" else '0';
	y175	<= data when address = "10101111" else '0';
	y176	<= data when address = "10110000" else '0';
	y177	<= data when address = "10110001" else '0';
	y178	<= data when address = "10110010" else '0';
	y179	<= data when address = "10110011" else '0';
	y180	<= data when address = "10110100" else '0';
	y181	<= data when address = "10110101" else '0';
	y182	<= data when address = "10110110" else '0';
	y183	<= data when address = "10110111" else '0';
	y184	<= data when address = "10111000" else '0';
	y185	<= data when address = "10111001" else '0';
	y186	<= data when address = "10111010" else '0';
	y187	<= data when address = "10111011" else '0';
	y188	<= data when address = "10111100" else '0';
	y189	<= data when address = "10111101" else '0';
	y190	<= data when address = "10111110" else '0';
	y191	<= data when address = "10111111" else '0';
	y192	<= data when address = "11000000" else '0';
	y193	<= data when address = "11000001" else '0';
	y194	<= data when address = "11000010" else '0';
	y195	<= data when address = "11000011" else '0';
	y196	<= data when address = "11000100" else '0';
	y197	<= data when address = "11000101" else '0';
	y198	<= data when address = "11000110" else '0';
	y199	<= data when address = "11000111" else '0';
	y200	<= data when address = "11001000" else '0';
	y201	<= data when address = "11001001" else '0';
	y202	<= data when address = "11001010" else '0';
	y203	<= data when address = "11001011" else '0';
	y204	<= data when address = "11001100" else '0';
	y205	<= data when address = "11001101" else '0';
	y206	<= data when address = "11001110" else '0';
	y207	<= data when address = "11001111" else '0';
	y208	<= data when address = "11010000" else '0';
	y209	<= data when address = "11010001" else '0';
	y210	<= data when address = "11010010" else '0';
	y211	<= data when address = "11010011" else '0';
	y212	<= data when address = "11010100" else '0';
	y213	<= data when address = "11010101" else '0';
	y214	<= data when address = "11010110" else '0';
	y215	<= data when address = "11010111" else '0';
	y216	<= data when address = "11011000" else '0';
	y217	<= data when address = "11011001" else '0';
	y218	<= data when address = "11011010" else '0';
	y219	<= data when address = "11011011" else '0';
	y220	<= data when address = "11011100" else '0';
	y221	<= data when address = "11011101" else '0';
	y222	<= data when address = "11011110" else '0';
	y223	<= data when address = "11011111" else '0';
	y224	<= data when address = "11100000" else '0';
	y225	<= data when address = "11100001" else '0';
	y226	<= data when address = "11100010" else '0';
	y227	<= data when address = "11100011" else '0';
	y228	<= data when address = "11100100" else '0';
	y229	<= data when address = "11100101" else '0';
	y230	<= data when address = "11100110" else '0';
	y231	<= data when address = "11100111" else '0';
	y232	<= data when address = "11101000" else '0';
	y233	<= data when address = "11101001" else '0';
	y234	<= data when address = "11101010" else '0';
	y235	<= data when address = "11101011" else '0';
	y236	<= data when address = "11101100" else '0';
	y237	<= data when address = "11101101" else '0';
	y238	<= data when address = "11101110" else '0';
	y239	<= data when address = "11101111" else '0';
	y240	<= data when address = "11110000" else '0';
	y241	<= data when address = "11110001" else '0';
	y242	<= data when address = "11110010" else '0';
	y243	<= data when address = "11110011" else '0';
	y244	<= data when address = "11110100" else '0';
	y245	<= data when address = "11110101" else '0';
	y246	<= data when address = "11110110" else '0';
	y247	<= data when address = "11110111" else '0';
	y248	<= data when address = "11111000" else '0';
	y249	<= data when address = "11111001" else '0';
	y250	<= data when address = "11111010" else '0';
	y251	<= data when address = "11111011" else '0';
	y252	<= data when address = "11111100" else '0';
	y253	<= data when address = "11111101" else '0';
	y254	<= data when address = "11111110" else '0';
	y255	<= data when address = "11111111" else '0';
end;

library	ieee;
use	ieee.std_logic_1164.all;

entity decoderNx256 is
generic(
	N:	positive
);
port(
	data:		in	std_logic_vector((N-1) downto 0);
	y0:			out	std_logic_vector((N-1) downto 0);
	y1:			out	std_logic_vector((N-1) downto 0);
	y2:			out	std_logic_vector((N-1) downto 0);
	y3:			out	std_logic_vector((N-1) downto 0);
	y4:			out	std_logic_vector((N-1) downto 0);
	y5:			out	std_logic_vector((N-1) downto 0);
	y6:			out	std_logic_vector((N-1) downto 0);
	y7:			out	std_logic_vector((N-1) downto 0);
	y8:			out	std_logic_vector((N-1) downto 0);
	y9:			out	std_logic_vector((N-1) downto 0);
	y10:		out	std_logic_vector((N-1) downto 0);
	y11:		out	std_logic_vector((N-1) downto 0);
	y12:		out	std_logic_vector((N-1) downto 0);
	y13:		out	std_logic_vector((N-1) downto 0);
	y14:		out	std_logic_vector((N-1) downto 0);
	y15:		out	std_logic_vector((N-1) downto 0);
	y16:		out	std_logic_vector((N-1) downto 0);
	y17:		out	std_logic_vector((N-1) downto 0);
	y18:		out	std_logic_vector((N-1) downto 0);
	y19:		out	std_logic_vector((N-1) downto 0);
	y20:		out	std_logic_vector((N-1) downto 0);
	y21:		out	std_logic_vector((N-1) downto 0);
	y22:		out	std_logic_vector((N-1) downto 0);
	y23:		out	std_logic_vector((N-1) downto 0);
	y24:		out	std_logic_vector((N-1) downto 0);
	y25:		out	std_logic_vector((N-1) downto 0);
	y26:		out	std_logic_vector((N-1) downto 0);
	y27:		out	std_logic_vector((N-1) downto 0);
	y28:		out	std_logic_vector((N-1) downto 0);
	y29:		out	std_logic_vector((N-1) downto 0);
	y30:		out	std_logic_vector((N-1) downto 0);
	y31:		out	std_logic_vector((N-1) downto 0);
	y32:		out	std_logic_vector((N-1) downto 0);
	y33:		out	std_logic_vector((N-1) downto 0);
	y34:		out	std_logic_vector((N-1) downto 0);
	y35:		out	std_logic_vector((N-1) downto 0);
	y36:		out	std_logic_vector((N-1) downto 0);
	y37:		out	std_logic_vector((N-1) downto 0);
	y38:		out	std_logic_vector((N-1) downto 0);
	y39:		out	std_logic_vector((N-1) downto 0);
	y40:		out	std_logic_vector((N-1) downto 0);
	y41:		out	std_logic_vector((N-1) downto 0);
	y42:		out	std_logic_vector((N-1) downto 0);
	y43:		out	std_logic_vector((N-1) downto 0);
	y44:		out	std_logic_vector((N-1) downto 0);
	y45:		out	std_logic_vector((N-1) downto 0);
	y46:		out	std_logic_vector((N-1) downto 0);
	y47:		out	std_logic_vector((N-1) downto 0);
	y48:		out	std_logic_vector((N-1) downto 0);
	y49:		out	std_logic_vector((N-1) downto 0);
	y50:		out	std_logic_vector((N-1) downto 0);
	y51:		out	std_logic_vector((N-1) downto 0);
	y52:		out	std_logic_vector((N-1) downto 0);
	y53:		out	std_logic_vector((N-1) downto 0);
	y54:		out	std_logic_vector((N-1) downto 0);
	y55:		out	std_logic_vector((N-1) downto 0);
	y56:		out	std_logic_vector((N-1) downto 0);
	y57:		out	std_logic_vector((N-1) downto 0);
	y58:		out	std_logic_vector((N-1) downto 0);
	y59:		out	std_logic_vector((N-1) downto 0);
	y60:		out	std_logic_vector((N-1) downto 0);
	y61:		out	std_logic_vector((N-1) downto 0);
	y62:		out	std_logic_vector((N-1) downto 0);
	y63:		out	std_logic_vector((N-1) downto 0);
	y64:		out	std_logic_vector((N-1) downto 0);
	y65:		out	std_logic_vector((N-1) downto 0);
	y66:		out	std_logic_vector((N-1) downto 0);
	y67:		out	std_logic_vector((N-1) downto 0);
	y68:		out	std_logic_vector((N-1) downto 0);
	y69:		out	std_logic_vector((N-1) downto 0);
	y70:		out	std_logic_vector((N-1) downto 0);
	y71:		out	std_logic_vector((N-1) downto 0);
	y72:		out	std_logic_vector((N-1) downto 0);
	y73:		out	std_logic_vector((N-1) downto 0);
	y74:		out	std_logic_vector((N-1) downto 0);
	y75:		out	std_logic_vector((N-1) downto 0);
	y76:		out	std_logic_vector((N-1) downto 0);
	y77:		out	std_logic_vector((N-1) downto 0);
	y78:		out	std_logic_vector((N-1) downto 0);
	y79:		out	std_logic_vector((N-1) downto 0);
	y80:		out	std_logic_vector((N-1) downto 0);
	y81:		out	std_logic_vector((N-1) downto 0);
	y82:		out	std_logic_vector((N-1) downto 0);
	y83:		out	std_logic_vector((N-1) downto 0);
	y84:		out	std_logic_vector((N-1) downto 0);
	y85:		out	std_logic_vector((N-1) downto 0);
	y86:		out	std_logic_vector((N-1) downto 0);
	y87:		out	std_logic_vector((N-1) downto 0);
	y88:		out	std_logic_vector((N-1) downto 0);
	y89:		out	std_logic_vector((N-1) downto 0);
	y90:		out	std_logic_vector((N-1) downto 0);
	y91:		out	std_logic_vector((N-1) downto 0);
	y92:		out	std_logic_vector((N-1) downto 0);
	y93:		out	std_logic_vector((N-1) downto 0);
	y94:		out	std_logic_vector((N-1) downto 0);
	y95:		out	std_logic_vector((N-1) downto 0);
	y96:		out	std_logic_vector((N-1) downto 0);
	y97:		out	std_logic_vector((N-1) downto 0);
	y98:		out	std_logic_vector((N-1) downto 0);
	y99:		out	std_logic_vector((N-1) downto 0);
	y100:		out	std_logic_vector((N-1) downto 0);
	y101:		out	std_logic_vector((N-1) downto 0);
	y102:		out	std_logic_vector((N-1) downto 0);
	y103:		out	std_logic_vector((N-1) downto 0);
	y104:		out	std_logic_vector((N-1) downto 0);
	y105:		out	std_logic_vector((N-1) downto 0);
	y106:		out	std_logic_vector((N-1) downto 0);
	y107:		out	std_logic_vector((N-1) downto 0);
	y108:		out	std_logic_vector((N-1) downto 0);
	y109:		out	std_logic_vector((N-1) downto 0);
	y110:		out	std_logic_vector((N-1) downto 0);
	y111:		out	std_logic_vector((N-1) downto 0);
	y112:		out	std_logic_vector((N-1) downto 0);
	y113:		out	std_logic_vector((N-1) downto 0);
	y114:		out	std_logic_vector((N-1) downto 0);
	y115:		out	std_logic_vector((N-1) downto 0);
	y116:		out	std_logic_vector((N-1) downto 0);
	y117:		out	std_logic_vector((N-1) downto 0);
	y118:		out	std_logic_vector((N-1) downto 0);
	y119:		out	std_logic_vector((N-1) downto 0);
	y120:		out	std_logic_vector((N-1) downto 0);
	y121:		out	std_logic_vector((N-1) downto 0);
	y122:		out	std_logic_vector((N-1) downto 0);
	y123:		out	std_logic_vector((N-1) downto 0);
	y124:		out	std_logic_vector((N-1) downto 0);
	y125:		out	std_logic_vector((N-1) downto 0);
	y126:		out	std_logic_vector((N-1) downto 0);
	y127:		out	std_logic_vector((N-1) downto 0);
	y128:		out	std_logic_vector((N-1) downto 0);
	y129:		out	std_logic_vector((N-1) downto 0);
	y130:		out	std_logic_vector((N-1) downto 0);
	y131:		out	std_logic_vector((N-1) downto 0);
	y132:		out	std_logic_vector((N-1) downto 0);
	y133:		out	std_logic_vector((N-1) downto 0);
	y134:		out	std_logic_vector((N-1) downto 0);
	y135:		out	std_logic_vector((N-1) downto 0);
	y136:		out	std_logic_vector((N-1) downto 0);
	y137:		out	std_logic_vector((N-1) downto 0);
	y138:		out	std_logic_vector((N-1) downto 0);
	y139:		out	std_logic_vector((N-1) downto 0);
	y140:		out	std_logic_vector((N-1) downto 0);
	y141:		out	std_logic_vector((N-1) downto 0);
	y142:		out	std_logic_vector((N-1) downto 0);
	y143:		out	std_logic_vector((N-1) downto 0);
	y144:		out	std_logic_vector((N-1) downto 0);
	y145:		out	std_logic_vector((N-1) downto 0);
	y146:		out	std_logic_vector((N-1) downto 0);
	y147:		out	std_logic_vector((N-1) downto 0);
	y148:		out	std_logic_vector((N-1) downto 0);
	y149:		out	std_logic_vector((N-1) downto 0);
	y150:		out	std_logic_vector((N-1) downto 0);
	y151:		out	std_logic_vector((N-1) downto 0);
	y152:		out	std_logic_vector((N-1) downto 0);
	y153:		out	std_logic_vector((N-1) downto 0);
	y154:		out	std_logic_vector((N-1) downto 0);
	y155:		out	std_logic_vector((N-1) downto 0);
	y156:		out	std_logic_vector((N-1) downto 0);
	y157:		out	std_logic_vector((N-1) downto 0);
	y158:		out	std_logic_vector((N-1) downto 0);
	y159:		out	std_logic_vector((N-1) downto 0);
	y160:		out	std_logic_vector((N-1) downto 0);
	y161:		out	std_logic_vector((N-1) downto 0);
	y162:		out	std_logic_vector((N-1) downto 0);
	y163:		out	std_logic_vector((N-1) downto 0);
	y164:		out	std_logic_vector((N-1) downto 0);
	y165:		out	std_logic_vector((N-1) downto 0);
	y166:		out	std_logic_vector((N-1) downto 0);
	y167:		out	std_logic_vector((N-1) downto 0);
	y168:		out	std_logic_vector((N-1) downto 0);
	y169:		out	std_logic_vector((N-1) downto 0);
	y170:		out	std_logic_vector((N-1) downto 0);
	y171:		out	std_logic_vector((N-1) downto 0);
	y172:		out	std_logic_vector((N-1) downto 0);
	y173:		out	std_logic_vector((N-1) downto 0);
	y174:		out	std_logic_vector((N-1) downto 0);
	y175:		out	std_logic_vector((N-1) downto 0);
	y176:		out	std_logic_vector((N-1) downto 0);
	y177:		out	std_logic_vector((N-1) downto 0);
	y178:		out	std_logic_vector((N-1) downto 0);
	y179:		out	std_logic_vector((N-1) downto 0);
	y180:		out	std_logic_vector((N-1) downto 0);
	y181:		out	std_logic_vector((N-1) downto 0);
	y182:		out	std_logic_vector((N-1) downto 0);
	y183:		out	std_logic_vector((N-1) downto 0);
	y184:		out	std_logic_vector((N-1) downto 0);
	y185:		out	std_logic_vector((N-1) downto 0);
	y186:		out	std_logic_vector((N-1) downto 0);
	y187:		out	std_logic_vector((N-1) downto 0);
	y188:		out	std_logic_vector((N-1) downto 0);
	y189:		out	std_logic_vector((N-1) downto 0);
	y190:		out	std_logic_vector((N-1) downto 0);
	y191:		out	std_logic_vector((N-1) downto 0);
	y192:		out	std_logic_vector((N-1) downto 0);
	y193:		out	std_logic_vector((N-1) downto 0);
	y194:		out	std_logic_vector((N-1) downto 0);
	y195:		out	std_logic_vector((N-1) downto 0);
	y196:		out	std_logic_vector((N-1) downto 0);
	y197:		out	std_logic_vector((N-1) downto 0);
	y198:		out	std_logic_vector((N-1) downto 0);
	y199:		out	std_logic_vector((N-1) downto 0);
	y200:		out	std_logic_vector((N-1) downto 0);
	y201:		out	std_logic_vector((N-1) downto 0);
	y202:		out	std_logic_vector((N-1) downto 0);
	y203:		out	std_logic_vector((N-1) downto 0);
	y204:		out	std_logic_vector((N-1) downto 0);
	y205:		out	std_logic_vector((N-1) downto 0);
	y206:		out	std_logic_vector((N-1) downto 0);
	y207:		out	std_logic_vector((N-1) downto 0);
	y208:		out	std_logic_vector((N-1) downto 0);
	y209:		out	std_logic_vector((N-1) downto 0);
	y210:		out	std_logic_vector((N-1) downto 0);
	y211:		out	std_logic_vector((N-1) downto 0);
	y212:		out	std_logic_vector((N-1) downto 0);
	y213:		out	std_logic_vector((N-1) downto 0);
	y214:		out	std_logic_vector((N-1) downto 0);
	y215:		out	std_logic_vector((N-1) downto 0);
	y216:		out	std_logic_vector((N-1) downto 0);
	y217:		out	std_logic_vector((N-1) downto 0);
	y218:		out	std_logic_vector((N-1) downto 0);
	y219:		out	std_logic_vector((N-1) downto 0);
	y220:		out	std_logic_vector((N-1) downto 0);
	y221:		out	std_logic_vector((N-1) downto 0);
	y222:		out	std_logic_vector((N-1) downto 0);
	y223:		out	std_logic_vector((N-1) downto 0);
	y224:		out	std_logic_vector((N-1) downto 0);
	y225:		out	std_logic_vector((N-1) downto 0);
	y226:		out	std_logic_vector((N-1) downto 0);
	y227:		out	std_logic_vector((N-1) downto 0);
	y228:		out	std_logic_vector((N-1) downto 0);
	y229:		out	std_logic_vector((N-1) downto 0);
	y230:		out	std_logic_vector((N-1) downto 0);
	y231:		out	std_logic_vector((N-1) downto 0);
	y232:		out	std_logic_vector((N-1) downto 0);
	y233:		out	std_logic_vector((N-1) downto 0);
	y234:		out	std_logic_vector((N-1) downto 0);
	y235:		out	std_logic_vector((N-1) downto 0);
	y236:		out	std_logic_vector((N-1) downto 0);
	y237:		out	std_logic_vector((N-1) downto 0);
	y238:		out	std_logic_vector((N-1) downto 0);
	y239:		out	std_logic_vector((N-1) downto 0);
	y240:		out	std_logic_vector((N-1) downto 0);
	y241:		out	std_logic_vector((N-1) downto 0);
	y242:		out	std_logic_vector((N-1) downto 0);
	y243:		out	std_logic_vector((N-1) downto 0);
	y244:		out	std_logic_vector((N-1) downto 0);
	y245:		out	std_logic_vector((N-1) downto 0);
	y246:		out	std_logic_vector((N-1) downto 0);
	y247:		out	std_logic_vector((N-1) downto 0);
	y248:		out	std_logic_vector((N-1) downto 0);
	y249:		out	std_logic_vector((N-1) downto 0);
	y250:		out	std_logic_vector((N-1) downto 0);
	y251:		out	std_logic_vector((N-1) downto 0);
	y252:		out	std_logic_vector((N-1) downto 0);
	y253:		out	std_logic_vector((N-1) downto 0);
	y254:		out	std_logic_vector((N-1) downto 0);
	y255:		out	std_logic_vector((N-1) downto 0);
	address:	in	std_logic_vector(7 downto 0)
);
end;

architecture struct_decoderNx256 of decoderNx256 is
component decoder1x256 is
port(
	data:		in	std_logic;
	y0:			out	std_logic;
	y1:			out	std_logic;
	y2:			out	std_logic;
	y3:			out	std_logic;
	y4:			out	std_logic;
	y5:			out	std_logic;
	y6:			out	std_logic;
	y7:			out	std_logic;
	y8:			out	std_logic;
	y9:			out	std_logic;
	y10:		out	std_logic;
	y11:		out	std_logic;
	y12:		out	std_logic;
	y13:		out	std_logic;
	y14:		out	std_logic;
	y15:		out	std_logic;
	y16:		out	std_logic;
	y17:		out	std_logic;
	y18:		out	std_logic;
	y19:		out	std_logic;
	y20:		out	std_logic;
	y21:		out	std_logic;
	y22:		out	std_logic;
	y23:		out	std_logic;
	y24:		out	std_logic;
	y25:		out	std_logic;
	y26:		out	std_logic;
	y27:		out	std_logic;
	y28:		out	std_logic;
	y29:		out	std_logic;
	y30:		out	std_logic;
	y31:		out	std_logic;
	y32:		out	std_logic;
	y33:		out	std_logic;
	y34:		out	std_logic;
	y35:		out	std_logic;
	y36:		out	std_logic;
	y37:		out	std_logic;
	y38:		out	std_logic;
	y39:		out	std_logic;
	y40:		out	std_logic;
	y41:		out	std_logic;
	y42:		out	std_logic;
	y43:		out	std_logic;
	y44:		out	std_logic;
	y45:		out	std_logic;
	y46:		out	std_logic;
	y47:		out	std_logic;
	y48:		out	std_logic;
	y49:		out	std_logic;
	y50:		out	std_logic;
	y51:		out	std_logic;
	y52:		out	std_logic;
	y53:		out	std_logic;
	y54:		out	std_logic;
	y55:		out	std_logic;
	y56:		out	std_logic;
	y57:		out	std_logic;
	y58:		out	std_logic;
	y59:		out	std_logic;
	y60:		out	std_logic;
	y61:		out	std_logic;
	y62:		out	std_logic;
	y63:		out	std_logic;
	y64:		out	std_logic;
	y65:		out	std_logic;
	y66:		out	std_logic;
	y67:		out	std_logic;
	y68:		out	std_logic;
	y69:		out	std_logic;
	y70:		out	std_logic;
	y71:		out	std_logic;
	y72:		out	std_logic;
	y73:		out	std_logic;
	y74:		out	std_logic;
	y75:		out	std_logic;
	y76:		out	std_logic;
	y77:		out	std_logic;
	y78:		out	std_logic;
	y79:		out	std_logic;
	y80:		out	std_logic;
	y81:		out	std_logic;
	y82:		out	std_logic;
	y83:		out	std_logic;
	y84:		out	std_logic;
	y85:		out	std_logic;
	y86:		out	std_logic;
	y87:		out	std_logic;
	y88:		out	std_logic;
	y89:		out	std_logic;
	y90:		out	std_logic;
	y91:		out	std_logic;
	y92:		out	std_logic;
	y93:		out	std_logic;
	y94:		out	std_logic;
	y95:		out	std_logic;
	y96:		out	std_logic;
	y97:		out	std_logic;
	y98:		out	std_logic;
	y99:		out	std_logic;
	y100:		out	std_logic;
	y101:		out	std_logic;
	y102:		out	std_logic;
	y103:		out	std_logic;
	y104:		out	std_logic;
	y105:		out	std_logic;
	y106:		out	std_logic;
	y107:		out	std_logic;
	y108:		out	std_logic;
	y109:		out	std_logic;
	y110:		out	std_logic;
	y111:		out	std_logic;
	y112:		out	std_logic;
	y113:		out	std_logic;
	y114:		out	std_logic;
	y115:		out	std_logic;
	y116:		out	std_logic;
	y117:		out	std_logic;
	y118:		out	std_logic;
	y119:		out	std_logic;
	y120:		out	std_logic;
	y121:		out	std_logic;
	y122:		out	std_logic;
	y123:		out	std_logic;
	y124:		out	std_logic;
	y125:		out	std_logic;
	y126:		out	std_logic;
	y127:		out	std_logic;
	y128:		out	std_logic;
	y129:		out	std_logic;
	y130:		out	std_logic;
	y131:		out	std_logic;
	y132:		out	std_logic;
	y133:		out	std_logic;
	y134:		out	std_logic;
	y135:		out	std_logic;
	y136:		out	std_logic;
	y137:		out	std_logic;
	y138:		out	std_logic;
	y139:		out	std_logic;
	y140:		out	std_logic;
	y141:		out	std_logic;
	y142:		out	std_logic;
	y143:		out	std_logic;
	y144:		out	std_logic;
	y145:		out	std_logic;
	y146:		out	std_logic;
	y147:		out	std_logic;
	y148:		out	std_logic;
	y149:		out	std_logic;
	y150:		out	std_logic;
	y151:		out	std_logic;
	y152:		out	std_logic;
	y153:		out	std_logic;
	y154:		out	std_logic;
	y155:		out	std_logic;
	y156:		out	std_logic;
	y157:		out	std_logic;
	y158:		out	std_logic;
	y159:		out	std_logic;
	y160:		out	std_logic;
	y161:		out	std_logic;
	y162:		out	std_logic;
	y163:		out	std_logic;
	y164:		out	std_logic;
	y165:		out	std_logic;
	y166:		out	std_logic;
	y167:		out	std_logic;
	y168:		out	std_logic;
	y169:		out	std_logic;
	y170:		out	std_logic;
	y171:		out	std_logic;
	y172:		out	std_logic;
	y173:		out	std_logic;
	y174:		out	std_logic;
	y175:		out	std_logic;
	y176:		out	std_logic;
	y177:		out	std_logic;
	y178:		out	std_logic;
	y179:		out	std_logic;
	y180:		out	std_logic;
	y181:		out	std_logic;
	y182:		out	std_logic;
	y183:		out	std_logic;
	y184:		out	std_logic;
	y185:		out	std_logic;
	y186:		out	std_logic;
	y187:		out	std_logic;
	y188:		out	std_logic;
	y189:		out	std_logic;
	y190:		out	std_logic;
	y191:		out	std_logic;
	y192:		out	std_logic;
	y193:		out	std_logic;
	y194:		out	std_logic;
	y195:		out	std_logic;
	y196:		out	std_logic;
	y197:		out	std_logic;
	y198:		out	std_logic;
	y199:		out	std_logic;
	y200:		out	std_logic;
	y201:		out	std_logic;
	y202:		out	std_logic;
	y203:		out	std_logic;
	y204:		out	std_logic;
	y205:		out	std_logic;
	y206:		out	std_logic;
	y207:		out	std_logic;
	y208:		out	std_logic;
	y209:		out	std_logic;
	y210:		out	std_logic;
	y211:		out	std_logic;
	y212:		out	std_logic;
	y213:		out	std_logic;
	y214:		out	std_logic;
	y215:		out	std_logic;
	y216:		out	std_logic;
	y217:		out	std_logic;
	y218:		out	std_logic;
	y219:		out	std_logic;
	y220:		out	std_logic;
	y221:		out	std_logic;
	y222:		out	std_logic;
	y223:		out	std_logic;
	y224:		out	std_logic;
	y225:		out	std_logic;
	y226:		out	std_logic;
	y227:		out	std_logic;
	y228:		out	std_logic;
	y229:		out	std_logic;
	y230:		out	std_logic;
	y231:		out	std_logic;
	y232:		out	std_logic;
	y233:		out	std_logic;
	y234:		out	std_logic;
	y235:		out	std_logic;
	y236:		out	std_logic;
	y237:		out	std_logic;
	y238:		out	std_logic;
	y239:		out	std_logic;
	y240:		out	std_logic;
	y241:		out	std_logic;
	y242:		out	std_logic;
	y243:		out	std_logic;
	y244:		out	std_logic;
	y245:		out	std_logic;
	y246:		out	std_logic;
	y247:		out	std_logic;
	y248:		out	std_logic;
	y249:		out	std_logic;
	y250:		out	std_logic;
	y251:		out	std_logic;
	y252:		out	std_logic;
	y253:		out	std_logic;
	y254:		out	std_logic;
	y255:		out	std_logic;
	address:	in	std_logic_vector(7 downto 0)
);

end component;

begin
	u1: for i in (N-1) downto 0 generate
		u:  decoder1x256 port map(
				data	=> data(i),
				y0		=> y0(i),
				y1		=> y1(i),
				y2		=> y2(i),
				y3		=> y3(i),
				y4		=> y4(i),
				y5		=> y5(i),
				y6		=> y6(i),
				y7		=> y7(i),
				y8		=> y8(i),
				y9		=> y9(i),
				y10		=> y10(i),
				y11		=> y11(i),
				y12		=> y12(i),
				y13		=> y13(i),
				y14		=> y14(i),
				y15		=> y15(i),
				y16		=> y16(i),
				y17		=> y17(i),
				y18		=> y18(i),
				y19		=> y19(i),
				y20		=> y20(i),
				y21		=> y21(i),
				y22		=> y22(i),
				y23		=> y23(i),
				y24		=> y24(i),
				y25		=> y25(i),
				y26		=> y26(i),
				y27		=> y27(i),
				y28		=> y28(i),
				y29		=> y29(i),
				y30		=> y30(i),
				y31		=> y31(i),
				y32		=> y32(i),
				y33		=> y33(i),
				y34		=> y34(i),
				y35		=> y35(i),
				y36		=> y36(i),
				y37		=> y37(i),
				y38		=> y38(i),
				y39		=> y39(i),
				y40		=> y40(i),
				y41		=> y41(i),
				y42		=> y42(i),
				y43		=> y43(i),
				y44		=> y44(i),
				y45		=> y45(i),
				y46		=> y46(i),
				y47		=> y47(i),
				y48		=> y48(i),
				y49		=> y49(i),
				y50		=> y50(i),
				y51		=> y51(i),
				y52		=> y52(i),
				y53		=> y53(i),
				y54		=> y54(i),
				y55		=> y55(i),
				y56		=> y56(i),
				y57		=> y57(i),
				y58		=> y58(i),
				y59		=> y59(i),
				y60		=> y60(i),
				y61		=> y61(i),
				y62		=> y62(i),
				y63		=> y63(i),
				y64		=> y64(i),
				y65		=> y65(i),
				y66		=> y66(i),
				y67		=> y67(i),
				y68		=> y68(i),
				y69		=> y69(i),
				y70		=> y70(i),
				y71		=> y71(i),
				y72		=> y72(i),
				y73		=> y73(i),
				y74		=> y74(i),
				y75		=> y75(i),
				y76		=> y76(i),
				y77		=> y77(i),
				y78		=> y78(i),
				y79		=> y79(i),
				y80		=> y80(i),
				y81		=> y81(i),
				y82		=> y82(i),
				y83		=> y83(i),
				y84		=> y84(i),
				y85		=> y85(i),
				y86		=> y86(i),
				y87		=> y87(i),
				y88		=> y88(i),
				y89		=> y89(i),
				y90		=> y90(i),
				y91		=> y91(i),
				y92		=> y92(i),
				y93		=> y93(i),
				y94		=> y94(i),
				y95		=> y95(i),
				y96		=> y96(i),
				y97		=> y97(i),
				y98		=> y98(i),
				y99		=> y99(i),
				y100	=> y100(i),
				y101	=> y101(i),
				y102	=> y102(i),
				y103	=> y103(i),
				y104	=> y104(i),
				y105	=> y105(i),
				y106	=> y106(i),
				y107	=> y107(i),
				y108	=> y108(i),
				y109	=> y109(i),
				y110	=> y110(i),
				y111	=> y111(i),
				y112	=> y112(i),
				y113	=> y113(i),
				y114	=> y114(i),
				y115	=> y115(i),
				y116	=> y116(i),
				y117	=> y117(i),
				y118	=> y118(i),
				y119	=> y119(i),
				y120	=> y120(i),
				y121	=> y121(i),
				y122	=> y122(i),
				y123	=> y123(i),
				y124	=> y124(i),
				y125	=> y125(i),
				y126	=> y126(i),
				y127	=> y127(i),
				y128	=> y128(i),
				y129	=> y129(i),
				y130	=> y130(i),
				y131	=> y131(i),
				y132	=> y132(i),
				y133	=> y133(i),
				y134	=> y134(i),
				y135	=> y135(i),
				y136	=> y136(i),
				y137	=> y137(i),
				y138	=> y138(i),
				y139	=> y139(i),
				y140	=> y140(i),
				y141	=> y141(i),
				y142	=> y142(i),
				y143	=> y143(i),
				y144	=> y144(i),
				y145	=> y145(i),
				y146	=> y146(i),
				y147	=> y147(i),
				y148	=> y148(i),
				y149	=> y149(i),
				y150	=> y150(i),
				y151	=> y151(i),
				y152	=> y152(i),
				y153	=> y153(i),
				y154	=> y154(i),
				y155	=> y155(i),
				y156	=> y156(i),
				y157	=> y157(i),
				y158	=> y158(i),
				y159	=> y159(i),
				y160	=> y160(i),
				y161	=> y161(i),
				y162	=> y162(i),
				y163	=> y163(i),
				y164	=> y164(i),
				y165	=> y165(i),
				y166	=> y166(i),
				y167	=> y167(i),
				y168	=> y168(i),
				y169	=> y169(i),
				y170	=> y170(i),
				y171	=> y171(i),
				y172	=> y172(i),
				y173	=> y173(i),
				y174	=> y174(i),
				y175	=> y175(i),
				y176	=> y176(i),
				y177	=> y177(i),
				y178	=> y178(i),
				y179	=> y179(i),
				y180	=> y180(i),
				y181	=> y181(i),
				y182	=> y182(i),
				y183	=> y183(i),
				y184	=> y184(i),
				y185	=> y185(i),
				y186	=> y186(i),
				y187	=> y187(i),
				y188	=> y188(i),
				y189	=> y189(i),
				y190	=> y190(i),
				y191	=> y191(i),
				y192	=> y192(i),
				y193	=> y193(i),
				y194	=> y194(i),
				y195	=> y195(i),
				y196	=> y196(i),
				y197	=> y197(i),
				y198	=> y198(i),
				y199	=> y199(i),
				y200	=> y200(i),
				y201	=> y201(i),
				y202	=> y202(i),
				y203	=> y203(i),
				y204	=> y204(i),
				y205	=> y205(i),
				y206	=> y206(i),
				y207	=> y207(i),
				y208	=> y208(i),
				y209	=> y209(i),
				y210	=> y210(i),
				y211	=> y211(i),
				y212	=> y212(i),
				y213	=> y213(i),
				y214	=> y214(i),
				y215	=> y215(i),
				y216	=> y216(i),
				y217	=> y217(i),
				y218	=> y218(i),
				y219	=> y219(i),
				y220	=> y220(i),
				y221	=> y221(i),
				y222	=> y222(i),
				y223	=> y223(i),
				y224	=> y224(i),
				y225	=> y225(i),
				y226	=> y226(i),
				y227	=> y227(i),
				y228	=> y228(i),
				y229	=> y229(i),
				y230	=> y230(i),
				y231	=> y231(i),
				y232	=> y232(i),
				y233	=> y233(i),
				y234	=> y234(i),
				y235	=> y235(i),
				y236	=> y236(i),
				y237	=> y237(i),
				y238	=> y238(i),
				y239	=> y239(i),
				y240	=> y240(i),
				y241	=> y241(i),
				y242	=> y242(i),
				y243	=> y243(i),
				y244	=> y244(i),
				y245	=> y245(i),
				y246	=> y246(i),
				y247	=> y247(i),
				y248	=> y248(i),
				y249	=> y249(i),
				y250	=> y250(i),
				y251	=> y251(i),
				y252	=> y252(i),
				y253	=> y253(i),
				y254	=> y254(i),
				y255	=> y255(i),
				address	=> address
			);
	end generate u1;
end;

library	ieee;
use		ieee.std_logic_1164.all;

entity counterN is
generic(
	N:	positive
);
port(
	clock:			in	std_logic;
	carry_in:		in	std_logic;
	clock_enable:	in	std_logic;
	resetn:			in	std_logic;
	output:			out	std_logic_vector((N-1) downto 0);
	carry_out:		out	std_logic
);
end;

architecture struct_counterN of counterN is
component incrementerN is
generic(
	N:	positive
);
port(
	input:		in	std_logic_vector((N-1) downto 0);
	carry_in:	in	std_logic;
	sum:		out	std_logic_vector((N-1) downto 0);
	carry_out:	out	std_logic
);
end component;

component andNbit is
generic(
	N:	positive
);
port(
	input:	in	std_logic_vector((N-1) downto 0);
	y:		out	std_logic
);
end component;

component synchronous_latchN is
generic(
	N:	positive
);
port(
	rstn:			in	std_logic;
	clock:			in	std_logic;
	clock_enable:	in	std_logic;
	d:				in	std_logic_vector((N-1) downto 0);
	q:				out	std_logic_vector((N-1) downto 0)
);
end component;

signal	counter_out:		std_logic_vector(N downto 0);

signal	incrementer_out:	std_logic_vector(N downto 0);

signal	all_ones:			std_logic;

begin
	u1: incrementerN
			generic map(
				N => N
			)
			port map(
				input => counter_out((N-1) downto 0),
				carry_in => carry_in,
				sum => incrementer_out((N-1) downto 0),
				carry_out => incrementer_out(N)
			);

	u2: synchronous_latchN
			generic map(
				N => (N + 1)
			)
			port map(
				rstn => resetn,
				clock => clock,
				clock_enable => clock_enable,
				d => incrementer_out,
				q => counter_out
			);

	u3: andNbit
			generic map(
				N => N
			)
			port map(
				input => counter_out((N-1) downto 0),
				y => all_ones
			);

	carry_out <= all_ones and carry_in;
	output <= counter_out((N-1) downto 0);
end;
