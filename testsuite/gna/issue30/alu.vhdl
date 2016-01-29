library	ieee;
use		ieee.std_logic_1164.all;

library work;
use		work.definitions.all;

entity ccf_operation is
port(
	flags_in:	in	std_logic_vector(7 downto 0);
	flags_out:	out	std_logic_vector(7 downto 0)
);
end;

-- Tested with Modelsim 2015/11/25, works.

architecture struct_ccf_operation of ccf_operation is
begin
	-- A point of disagreement has been found between the Z80 user manual
	-- and Lance Levinthal's book entitled "Z80 Assembly Language Programming".
	-- The Z80 user manual says the half-carry bit gets the previous carry;
	-- Levinthal says the half-carry bit is unchanged.  For now, go with
	-- Levinthal's version as the Z80 users manual is inconsistent with
	-- itself on other instructions.  At this time, no such inconsistencies
	-- have been found with Levinthal's work.

	flags_out <= (	carry_bit => not flags_in(carry_bit),
					half_carry_bit => flags_in(carry_bit),
					others => '0');
end;

library	ieee;
use		ieee.std_logic_1164.all;

library work;
use		work.definitions.all;

entity sll8bit is
port(
	operand:	in	std_logic_vector(7 downto 0);
	output:		out	std_logic_vector(7 downto 0);
	flags_out:	out	std_logic_vector(7 downto 0)
);
end;

-- Tested with Modelsim 2015/11/25, works.

architecture struct_sll8bit of sll8bit is

signal	sll_result:	std_logic_vector(7 downto 0);
begin
	-- This operation is not documented by Zilog, but seems to work in their
	-- finished chip.  This code may not work the same way as the Z80 hardware
	-- works.  The functionality is assumed from the SRL instruction.

	sll_result <= operand(6 downto 0) & '0';

	output <= sll_result;
	flags_out <= (	carry_bit => operand(7),
					zero_bit => not (sll_result(7) or sll_result(6) or sll_result(5) or sll_result(4) or
									 sll_result(3) or sll_result(2) or sll_result(1) or sll_result(0)),
					parity_overflow_bit => not (sll_result(7) xor sll_result(6) xor sll_result(5) xor
												sll_result(4) xor sll_result(3) xor sll_result(2) xor
												sll_result(1) xor sll_result(0)),
					sign_bit => operand(6),
					others => '0');
end;

library	ieee;
use		ieee.std_logic_1164.all;

library work;
use		work.definitions.all;

entity srl8bit is
port(
	operand:	in	std_logic_vector(7 downto 0);
	output:		out	std_logic_vector(7 downto 0);
	flags_out:	out	std_logic_vector(7 downto 0)
);
end;

-- Tested with Modelsim 2015/11/25, works.

architecture struct_srl8bit of srl8bit is

signal	srl_result:	std_logic_vector(7 downto 0);

begin
	srl_result <= '0' & operand(7 downto 1);

	output <= srl_result;
	flags_out <= (	carry_bit => operand(0),
					zero_bit => not (srl_result(7) or srl_result(6) or srl_result(5) or srl_result(4) or
									 srl_result(3) or srl_result(2) or srl_result(1) or srl_result(0)),
					parity_overflow_bit => not (srl_result(7) xor srl_result(6) xor srl_result(5) xor
												srl_result(4) xor srl_result(3) xor srl_result(2) xor
												srl_result(1) xor srl_result(0)),
					others => '0');
end;

library	ieee;
use		ieee.std_logic_1164.all;

library work;
use		work.definitions.all;

entity and8bit is
port(
	operand1:	in	std_logic_vector(7 downto 0);
	operand2:	in	std_logic_vector(7 downto 0);
	output:		out	std_logic_vector(7 downto 0);
	flags_out:	out	std_logic_vector(7 downto 0)
);
end;

-- Tested with Modelsim 2015/11/25, works.

architecture struct_and8bit of and8bit is

signal	and_result:		std_logic_vector(7 downto 0);

begin
	and_result <= operand1 and operand2;

	flags_out <= (	sign_bit => and_result(7),
					zero_bit => not (and_result(7) or and_result(6) or and_result(5) or and_result(4) or
									 and_result(3) or and_result(2) or and_result(1) or and_result(0)),
					half_carry_bit => '1',
					parity_overflow_bit => not (and_result(7) xor and_result(6) xor and_result(5) xor
												and_result(4) xor and_result(3) xor and_result(2) xor
												and_result(1) xor and_result(0)),
					others => '0');

	output <= and_result;
end;

library	ieee;
use		ieee.std_logic_1164.all;

library	work;
use		work.definitions.all;

entity or8bit is
port(
	operand1:	in	std_logic_vector(7 downto 0);
	operand2:	in	std_logic_vector(7 downto 0);
	output:		out	std_logic_vector(7 downto 0);
	flags_out:	out	std_logic_vector(7 downto 0)
);
end;

-- Tested with Modelsim 2015/11/25, works.

architecture struct_or8bit of or8bit is

signal	or_result:		std_logic_vector(7 downto 0);

begin
	or_result <= operand1 or operand2;

	output <= or_result;
	flags_out <= (	sign_bit => or_result(7),
					half_carry_bit => '1',
					zero_bit => not (or_result(7) or or_result(6) or or_result(5) or or_result(4) or
									 or_result(3) or or_result(2) or or_result(1) or or_result(0)),
					parity_overflow_bit => not (or_result(7) xor or_result(6) xor or_result(5) xor
												or_result(4) xor or_result(3) xor or_result(2) xor
												or_result(1) xor or_result(0)),
					others => '0');
end;


library	ieee;
use		ieee.std_logic_1164.all;

library	work;
use		work.definitions.all;

entity sra8bit is
port(
	operand:	in	std_logic_vector(7 downto 0);
	output:		out	std_logic_vector(7 downto 0);
	flags_out:	out	std_logic_vector(7 downto 0)
);
end;

-- Tested with Modelsim 2015/11/25, works.

architecture struct_sra8bit of sra8bit is

signal	sra_result:	std_logic_vector(7 downto 0);

begin
	sra_result <= operand(7) & operand(7 downto 1);

	output <= sra_result;
	flags_out <= (	carry_bit => operand(0),
					zero_bit => not (sra_result(7) or sra_result(6) or sra_result(5) or sra_result(4) or
									 sra_result(3) or sra_result(2) or sra_result(1) or sra_result(0)),
					parity_overflow_bit => not (sra_result(7) xor sra_result(6) xor sra_result(5) xor
												sra_result(4) xor sra_result(3) xor sra_result(2) xor
												sra_result(1) xor sra_result(0)),
					sign_bit => operand(7),
					others => '0');
end;

library	ieee;
use		ieee.std_logic_1164.all;

library	work;
use		work.definitions.all;

entity xor8bit is
port(
	operand1:	in	std_logic_vector(7 downto 0);
	operand2:	in	std_logic_vector(7 downto 0);
	output:		out	std_logic_vector(7 downto 0);
	flags_out:	out	std_logic_vector(7 downto 0)
);
end;

-- Tested with Modelsim 2015/11/25, works.

architecture struct_xor8bit of xor8bit is

signal	xor_result:		std_logic_vector(7 downto 0);

begin
	xor_result <= operand1 xor operand2;

	output <= xor_result;
	flags_out <= (	sign_bit => xor_result(7),
					half_carry_bit => '1',
					zero_bit => not (xor_result(7) or xor_result(6) or xor_result(5) or xor_result(4) or
									 xor_result(3) or xor_result(2) or xor_result(1) or xor_result(0)),
					parity_overflow_bit => not (xor_result(7) xor xor_result(6) xor xor_result(5) xor
												xor_result(4) xor xor_result(3) xor xor_result(2) xor
												xor_result(1) xor xor_result(0)),
					others => '0');
end;

library	ieee;
use		ieee.std_logic_1164.all;

library	work;
use		work.definitions.all;

entity sla8bit is
port(
	operand:	in	std_logic_vector(7 downto 0);
	output:		out	std_logic_vector(7 downto 0);
	flags_out:	out	std_logic_vector(7 downto 0)
);
end;

-- Tested with Modelsim 2015/11/25, works.

architecture struct_sla8bit of sla8bit is

signal	sla_result:	std_logic_vector(7 downto 0);

begin
	sla_result <= operand(6 downto 0) & '0';

	output <= sla_result;
	flags_out <= (	sign_bit => sla_result(7),
					half_carry_bit => '1',
					zero_bit => not (sla_result(7) or sla_result(6) or sla_result(5) or sla_result(4) or
									 sla_result(3) or sla_result(2) or sla_result(1) or sla_result(0)),
					parity_overflow_bit => not (sla_result(7) xor sla_result(6) xor sla_result(5) xor
												sla_result(4) xor sla_result(3) xor sla_result(2) xor
												sla_result(1) xor sla_result(0)),
					others => '0');
end;

library	ieee;
use		ieee.std_logic_1164.all;

entity subtractor is
port(
	minuend, subtrahend:	in	std_logic;
	borrow_in:				in	std_logic;
	difference:				out	std_logic;
	borrow_out:				out	std_logic
);
end;

architecture struct_subtractor of subtractor is
begin

-- These expressions were derived from the truth table of a single bit subtractor and simplified with
-- a Karnaugh map.

	difference <=	(borrow_in and (not minuend) and (not subtrahend)) or
					((not borrow_in) and (not minuend) and subtrahend) or
					(borrow_in and minuend and subtrahend) or
					((not borrow_in) and minuend and (not subtrahend));

	borrow_out <=	(not minuend and subtrahend) or
					(borrow_in and (not minuend)) or
					(borrow_in and subtrahend);
end;

library	ieee;
use		ieee.std_logic_1164.all;

entity subtractorN is
generic(
	N:	positive
);
port(
	minuend:	in	std_logic_vector((N-1) downto 0);
	subtrahend:	in	std_logic_vector((N-1) downto 0);
	borrow_in:	in	std_logic;
	difference:	out	std_logic_vector((N-1) downto 0);
	borrow_out:	out	std_logic
);
end;

architecture struct_subtractorN of subtractorN is
component subtractor is
port(
	minuend, subtrahend:	in	std_logic;
	borrow_in:				in	std_logic;
	difference:				out	std_logic;
	borrow_out:				out	std_logic
);
end component;

signal	borrow:	std_logic_vector(N downto 0);

begin
--	These expressions were derived from the truth table of a single bit subtractor and simplified with a
--	Karnaugh map.
--	d = difference, m = minuend, s = subtrahend, b = borrow
--
--	d(i) =	(b(i) and (not m(i)) and (not s(i))) or
--			((not b(i)) and (not m(i)) and s(i)) or
--			(b(i) and m(i) and s(i)) or
--			((not b(i)) and m(i) and (not s(i)))
--
--	b(i+1) =	(not m(i) and s(i)) or
--				(b(i) and (not m(i))) or
--				(b(i) and s(i)

	borrow(0) <= borrow_in;

	u1: for i in 0 to (N-1) generate
		u: subtractor port map(
				minuend => minuend(i),
				subtrahend => subtrahend(i),
				borrow_in => borrow(i),
				difference => difference(i),
				borrow_out => borrow(i+1)
			);
		end generate;

	borrow_out <= borrow(N);
end;

library	ieee;
use		ieee.std_logic_1164.all;

library	work;
use		work.definitions.all;

entity subtractor8x2 is
port(
	minuend, subtrahend:	in	std_logic_vector(7 downto 0);
	borrow_in:				in	std_logic;
	difference:				out	std_logic_vector(7 downto 0);
	flags_out:				out	std_logic_vector(7 downto 0)
);
end;

-- Tested with Modelsim 2015/11/25, works.

architecture struct_subtractor8x2 of subtractor8x2 is
component subtractor is
port(
	minuend, subtrahend:	in	std_logic;
	borrow_in:				in	std_logic;
	difference:				out	std_logic;
	borrow_out:				out	std_logic
);
end component;

signal	borrow:	std_logic_vector(8 downto 0);
signal	d:		std_logic_vector(7 downto 0);

begin
	borrow(0) <= borrow_in;

	u1: for i in 0 to 7 generate
		u: subtractor port map(
				minuend => minuend(i),
				subtrahend => subtrahend(i),
				borrow_in => borrow(i),
				difference => d(i),
				borrow_out => borrow(i+1)
			);
		end generate;

	difference <= d;

	flags_out <= (	sign_bit => d(7),
					zero_bit => not (d(0) or d(1) or d(2) or d(3) or d(4) or d(5) or d(6) or d(7)),
					half_carry_bit => borrow(4),
					parity_overflow_bit => (minuend(7) xor subtrahend(7)) and (minuend(7) xor d(7)),
					add_sub_bit => '1',
					carry_bit => borrow(8),
					others => '0');
end;

library	ieee;
use		ieee.std_logic_1164.all;

entity adder is
port(
	addend, augend:	in	std_logic;
	carry_in:		in	std_logic;
	sum:			out	std_logic;
	carry_out:		out	std_logic
);
end;

architecture struct_adder of adder is
begin
--	These expressions are derived from a single bit full adder truth table and simplified with a
--	Karnaugh map.
	sum <=	((not (carry_in)) and (not addend) and augend) or
			((not carry_in) and addend and (not augend)) or
			(carry_in and (not addend) and (not augend)) or
			(carry_in and addend and augend);

	carry_out <=	(addend and augend) or
					(carry_in and addend) or
					(carry_in and augend);
end;

library	ieee;
use		ieee.std_logic_1164.all;

entity adderN is
generic(
	N:	positive
);
port(
	addend:		in	std_logic_vector((N-1) downto 0);
	augend:		in	std_logic_vector((N-1) downto 0);
	carry_in:	in	std_logic;
	sum:		out	std_logic_vector((N-1) downto 0);
	carry_out:	out	std_logic
);
end;

-- Tested with Modelsim 2015/12/11, works.

architecture struct_adderN of adderN is
component adder is
port(
	addend, augend:	in	std_logic;
	carry_in:		in	std_logic;
	sum:			out	std_logic;
	carry_out:		out	std_logic
);
end component;

signal	carry:	std_logic_vector(N downto 0);

begin
	carry(0) <= carry_in;

	u1: for i in 0 to (N-1) generate
		u: adder port map(
				addend => addend(i),
				augend => augend(i),
				carry_in => carry(i),
				sum => sum(i),
				carry_out => carry(i+1)
			);
		end generate;

	carry_out <= carry(N);
end;

library	ieee;
use		ieee.std_logic_1164.all;

library	work;
use		work.definitions.all;

entity adder8x2 is
port(
	addend, augend:		in	std_logic_vector(7 downto 0);
	carry_in:			in	std_logic;
	sum:				out	std_logic_vector(7 downto 0);
	flags_out:			out	std_logic_vector(7 downto 0)
);
end;

-- Tested with Modelsim 2015/11/25, works.
-- The adderN version is not used because access to carry out of bit 3 is required.

architecture struct_adder8x2 of adder8x2 is
component adder is
port(
	addend, augend:	in	std_logic;
	carry_in:		in	std_logic;
	sum:			out	std_logic;
	carry_out:		out	std_logic
);
end component;

signal	result:	std_logic_vector(7 downto 0);
signal	carry:	std_logic_vector(8 downto 0);

begin
	carry(0) <= carry_in;

	u1: for i in 0 to 7 generate
		u: adder port map(
				addend => addend(i),
				augend => augend(i),
				carry_in => carry(i),
				sum => result(i),
				carry_out => carry(i+1)
			);
		end generate;

	sum <= result;
	flags_out <= (	sign_bit => result(7),
					zero_bit => not (result(7) or result(6) or result(5) or result(4) or
									 result(3) or result(2) or result(1) or result(0)),
					half_carry_bit => carry(4),
					parity_overflow_bit => not	(addend(7) xor augend(7)) and
												(addend(7) xor result(7)),
					add_sub_bit => '0',
					carry_bit => carry(8),
					others => '0');
end;

library	ieee;
use		ieee.std_logic_1164.all;

library	work;
use		work.definitions.all;

entity cpl is
port(
	operand:	in	std_logic_vector(7 downto 0);
	output:		out	std_logic_vector(7 downto 0);
	flags_out:	out	std_logic_vector(7 downto 0)
);
end;

-- Tested with Modelsim 2015/11/25, works.

architecture struct_cpl of cpl is
begin
	output <= not operand;
	flags_out <= (	half_carry_bit => '1',
					add_sub_bit => '1',
					others => '0');
end;

library	ieee;
use		ieee.std_logic_1164.all;

library	ieee;
use		ieee.std_logic_1164.all;

library	work;
use		work.definitions.all;

entity rlc8bit is
port(
	operand:	in	std_logic_vector(7 downto 0);
	output:		out	std_logic_vector(7 downto 0);
	flags_out:	out	std_logic_vector(7 downto 0)
);
end;

-- Tested with Modelsim 2015/11/25, works.

architecture struct_rlc8bit of rlc8bit is

signal	rlc_result:	std_logic_vector(7 downto 0);

begin
	rlc_result(7 downto 1) <= operand(6 downto 0);
	rlc_result(0) <= operand(7);

	output <= rlc_result;
	flags_out <= (	carry_bit => operand(7),
					parity_overflow_bit => not (rlc_result(7) xor rlc_result(6) xor rlc_result(5) xor
												rlc_result(4) xor rlc_result(3) xor rlc_result(2) xor
												rlc_result(1) xor rlc_result(0)),
					zero_bit => not (rlc_result(7) or rlc_result(6) or rlc_result(5) or rlc_result(4) or
									 rlc_result(3) or rlc_result(2) or rlc_result(1) or rlc_result(0)),
					sign_bit => rlc_result(7),
					others => '0');
end;

library	ieee;
use		ieee.std_logic_1164.all;

library	work;
use		work.definitions.all;

entity rrc8bit is
port(
	operand:	in	std_logic_vector(7 downto 0);
	output:		out	std_logic_vector(7 downto 0);
	flags_out:	out	std_logic_vector(7 downto 0)
);
end;

-- Tested with Modelsim 2015/11/25, works.

architecture struct_rrc8bit of rrc8bit is

signal	rrc_result:	std_logic_vector(7 downto 0);

begin
	rrc_result(6 downto 0) <= operand(7 downto 1);
	rrc_result(7) <= operand(0);

	output <= rrc_result;
	flags_out <= (	carry_bit => operand(0),
					zero_bit => not (rrc_result(7) or rrc_result(6) or rrc_result(5) or rrc_result(4) or
									 rrc_result(3) or rrc_result(2) or rrc_result(1) or rrc_result(0)),
					parity_overflow_bit => not (rrc_result(7) xor rrc_result(6) xor rrc_result(5) xor
												rrc_result(4) xor rrc_result(3) xor rrc_result(2) xor
												rrc_result(1) xor rrc_result(0)),
					sign_bit => operand(0),
					others => '0');
end;

library	ieee;
use		ieee.std_logic_1164.all;

library	work;
use		work.definitions.all;

entity rl8bit is
port(
	operand:	in	std_logic_vector(7 downto 0);
	carry_in:	in	std_logic;
	output:		out	std_logic_vector(7 downto 0);
	flags_out:	out	std_logic_vector(7 downto 0)
);
end;

-- Tested with Modelsim 2015/11/25, works.

architecture struct_rl8bit of rl8bit is

signal	rl_result:	std_logic_vector(7 downto 0);

begin
	rl_result (7 downto 1) <= operand(6 downto 0);
	rl_result(0) <= carry_in;

	output <= rl_result;
	flags_out <= (	carry_bit => operand(7),
					zero_bit => not (rl_result(7) or rl_result(6) or rl_result(5) or rl_result(4) or
									 rl_result(3) or rl_result(2) or rl_result(1) or rl_result(0)),
					parity_overflow_bit => not ((rl_result(7) xor rl_result(6) xor rl_result(5) xor
												 rl_result(4) xor rl_result(3) xor rl_result(2) xor
												 rl_result(1) xor rl_result(0))),
					sign_bit => operand(6),
					others => '0');
end;

library	ieee;
use		ieee.std_logic_1164.all;

library	work;
use		work.definitions.all;

entity rr8bit is
port(
	operand:	in	std_logic_vector(7 downto 0);
	carry_in:	in	std_logic;
	output:		out	std_logic_vector(7 downto 0);
	flags_out:	out	std_logic_vector(7 downto 0)
);
end;

-- Tested with Modelsim 2015/11/25, works.

architecture struct_rr8bit of rr8bit is

signal	rr_result:	std_logic_vector(7 downto 0);

begin
	rr_result(6 downto 0) <= operand(7 downto 1);
	rr_result(7) <= carry_in;

	output <= rr_result;
	flags_out <= (	carry_bit => operand(0),
					zero_bit => not (rr_result(7) or rr_result(6) or rr_result(5) or rr_result(4) or
									 rr_result(3) or rr_result(2) or rr_result(1) or rr_result(0)),
					parity_overflow_bit => not (rr_result(7) xor rr_result(6) xor rr_result(5) xor
												rr_result(4) xor rr_result(3) xor rr_result(2) xor
												rr_result(1) xor rr_result(0)),
					sign_bit => carry_in,
					others => '0');
end;

library	ieee;
use		ieee.std_logic_1164.all;

entity daa is
port(
	operand:	in	std_logic_vector(7 downto 0);
	flags_in:	in	std_logic_vector(7 downto 0);
	output:		out	std_logic_vector(7 downto 0);
	flags_out:	out	std_logic_vector(7 downto 0)
);
end;

-- Untested, this is nothing more than a stub with code to prevent unassigned variable warnings/errors.

architecture struct_daa of daa is
begin
	output <= operand;
	flags_out <= flags_in;
end;

library	ieee;
use		ieee.std_logic_1164.all;

library	work;
use		work.definitions.all;

entity bit_op is
port(
	operand1:	in	std_logic_vector(7 downto 0);
	operand2:	in	std_logic_vector(7 downto 0);
	flags_out:	out	std_logic_vector(7 downto 0)
);
end;

-- Tested with Modelsim 2015/11/25, works.

architecture struct_bit_op of bit_op is

signal	zero:	std_logic;

begin
	zero <= '1' when (operand1 and operand2) = x"00" else '0';

	flags_out <= (	zero_bit => zero,
					half_carry_bit => '1',
					others => '0');
end;

library	ieee;
use		ieee.std_logic_1164.all;

library	work;
use		work.definitions.all;

entity rld is
port(
	primary_op:			in	std_logic_vector(7 downto 0);
	secondary_op:		in	std_logic_vector(7 downto 0);
	result:				out	std_logic_vector(7 downto 0);
	secondary_result:	out	std_logic_vector(7 downto 0);
	flags_out:			out	std_logic_vector(7 downto 0)
);
end;

-- Tested with Modelsim 2015/11/25, works.

architecture struct_rld of rld is

signal	primary_result:	std_logic_vector(7 downto 0);

begin
	primary_result(7 downto 4) <= primary_op(7 downto 4);
	primary_result(3 downto 0) <= secondary_op(7 downto 4);
	result <= primary_result;
	secondary_result(7 downto 4) <= secondary_op(3 downto 0);
	secondary_result(3 downto 0) <= primary_op(3 downto 0);
	flags_out <= (	sign_bit => primary_result(7),
					zero_bit => not (primary_result(7) or primary_result(6) or primary_result(5) or
									 primary_result(4) or primary_result(3) or primary_result(2) or
									 primary_result(1) or primary_result(0)),
					parity_overflow_bit => not (primary_result(7) xor primary_result(6) xor
												primary_result(5) xor primary_result(4) xor
												primary_result(3) xor primary_result(2) xor
												primary_result(1) xor primary_result(0)),
					others => '0');
end;

library	ieee;
use		ieee.std_logic_1164.all;

library	work;
use		work.definitions.all;

entity rrd is
port(
	primary_op:			in	std_logic_vector(7 downto 0);
	secondary_op:		in	std_logic_vector(7 downto 0);
	result:				out	std_logic_vector(7 downto 0);
	secondary_result:	out	std_logic_vector(7 downto 0);
	flags_out:			out	std_logic_vector(7 downto 0)
);
end;

-- Tested with Modelsim 2015/11/25, works.

architecture struct_rrd of rrd is

signal	primary_result:	std_logic_vector(7 downto 0);

begin
	primary_result(7 downto 4) <= primary_op(7 downto 4);
	primary_result(3 downto 0) <= secondary_op(3 downto 0);
	result <= primary_result;
	secondary_result(7 downto 4) <= primary_op(3 downto 0);
	secondary_result(3 downto 0) <= secondary_op(7 downto 4);
	flags_out <= (	sign_bit => primary_result(7),
					zero_bit => not (primary_result(7) or primary_result(6) or primary_result(5) or
									 primary_result(4) or primary_result(3) or primary_result(2) or
									 primary_result(1) or primary_result(0)),
					parity_overflow_bit => not (primary_result(7) xor primary_result(6) xor
												primary_result(5) xor primary_result(4) xor
												primary_result(3) xor primary_result(2) xor
												primary_result(1) xor primary_result(0)),
					others => '0');

end;

library	ieee;
use		ieee.std_logic_1164.all;

library	work;
use		work.definitions.all;

entity in_rc_flags is
port(
	operand:	in	std_logic_vector(7 downto 0);
	flags_out:	out	std_logic_vector(7 downto 0)
);
end;

-- Tested with Modelsim 2015/11/25, works.

architecture struct_in_rc_flags of in_rc_flags is
begin
	flags_out <= (	zero_bit => not (operand(7) or operand(6) or operand(5) or operand(4) or
									 operand(3) or operand(2) or operand(1) or operand(0)),
					sign_bit => operand(7),
					parity_overflow_bit => not (operand(7) xor operand(6) xor operand(5) xor
												operand(4) xor operand(3) xor operand(2) xor
												operand(1) xor operand(0)),
					others => '0');
end;

library	ieee;
use		ieee.std_logic_1164.all;

library	work;
use		work.definitions.all;

entity bmtc is
port(
	operand1:	in	std_logic_vector(7 downto 0);
	operand2:	in	std_logic_vector(7 downto 0);
	output:		out	std_logic_vector(7 downto 0);
	flags_out:	out	std_logic_vector(7 downto 0)
);
end;

-- Tested with Modelsim 2015/11/25, works.

architecture struct_bmtc of bmtc is

signal	result:	std_logic_vector(7 downto 0);

begin
	result <= operand1 or operand2;
	output <= result;
	flags_out <= (	parity_overflow_bit => not (result(7) or result(6) or result(5) or result(4) or
												result(3) or result(2) or result(1) or result(0)),
					others => '0');
end;

library	ieee;
use		ieee.std_logic_1164.all;

library	work;
use		work.definitions.all;

entity alu is
port(
	-- control
	operation:			in	std_logic_vector(4 downto 0);

	-- operands
	primary_operand:	in	std_logic_vector(7 downto 0);
	secondary_operand:	in	std_logic_vector(7 downto 0);
	flags_in:			in	std_logic_vector(7 downto 0);

	--	results
	output, flags_out:	out	std_logic_vector(7 downto 0);
	secondary_out:		out	std_logic_vector(7 downto 0)
);
end;

-- Tested 2016/11/22, works on Modelsim simulator along with all components.

architecture struct_alu of alu is
component bmtc is
port(
	operand1:	in	std_logic_vector(7 downto 0);
	operand2:	in	std_logic_vector(7 downto 0);
	output:		out	std_logic_vector(7 downto 0);
	flags_out:	out	std_logic_vector(7 downto 0)
);
end component;

component srl8bit is
port(
	operand:	in	std_logic_vector(7 downto 0);
	output:		out	std_logic_vector(7 downto 0);
	flags_out:	out	std_logic_vector(7 downto 0)
);
end component;

component sll8bit is
port(
	operand:	in	std_logic_vector(7 downto 0);
	output:		out	std_logic_vector(7 downto 0);
	flags_out:	out	std_logic_vector(7 downto 0)
);
end component;

component sra8bit is
port(
	operand:	in	std_logic_vector(7 downto 0);
	output:		out	std_logic_vector(7 downto 0);
	flags_out:	out	std_logic_vector(7 downto 0)
);
end component;

component sla8bit is
port(
	operand:	in	std_logic_vector(7 downto 0);
	output:		out	std_logic_vector(7 downto 0);
	flags_out:	out	std_logic_vector(7 downto 0)
);
end component;

component in_rc_flags is
port(
	operand:	in	std_logic_vector(7 downto 0);
	flags_out:	out	std_logic_vector(7 downto 0)
);
end component;

component ccf_operation is
port(
	flags_in:	in	std_logic_vector(7 downto 0);
	flags_out:	out	std_logic_vector(7 downto 0)
);
end component;

component cpl is
port(
	operand:	in	std_logic_vector(7 downto 0);
	output:		out	std_logic_vector(7 downto 0);
	flags_out:	out	std_logic_vector(7 downto 0)
);
end component;

component xor8bit is
port(
	operand1:	in	std_logic_vector(7 downto 0);
	operand2:	in	std_logic_vector(7 downto 0);
	output:		out	std_logic_vector(7 downto 0);
	flags_out:	out	std_logic_vector(7 downto 0)
);
end component;

component or8bit is
port(
	operand1:	in	std_logic_vector(7 downto 0);
	operand2:	in	std_logic_vector(7 downto 0);
	output:		out	std_logic_vector(7 downto 0);
	flags_out:	out	std_logic_vector(7 downto 0)
);
end component;

component and8bit is
port(
	operand1:	in	std_logic_vector(7 downto 0);
	operand2:	in	std_logic_vector(7 downto 0);
	output:		out	std_logic_vector(7 downto 0);
	flags_out:	out	std_logic_vector(7 downto 0)
);
end component;

component subtractor8x2 is
port(
	minuend, subtrahend:	in	std_logic_vector(7 downto 0);
	borrow_in:				in	std_logic;
	difference:				out	std_logic_vector(7 downto 0);
	flags_out:				out	std_logic_vector(7 downto 0)
);
end component;

component adder8x2 is
port(
	addend, augend:		in	std_logic_vector(7 downto 0);
	carry_in:			in	std_logic;
	sum:				out	std_logic_vector(7 downto 0);
	flags_out:			out	std_logic_vector(7 downto 0)
);
end component;

component magnitudeN is
generic(
	N:	positive
);
port(
	a:		in	std_logic_vector((N-1) downto 0);
	b:		in	std_logic_vector((N-1) downto 0);
	equal:	out	std_logic;
	lt:		out	std_logic; 	-- '1' if a < b
	gt:		out	std_logic	-- '1' if a > b
);
end component;

component rrd is
port(
	primary_op:			in	std_logic_vector(7 downto 0);
	secondary_op:		in	std_logic_vector(7 downto 0);
	result:				out	std_logic_vector(7 downto 0);
	secondary_result:	out	std_logic_vector(7 downto 0);
	flags_out:			out	std_logic_vector(7 downto 0)
);
end component;

component rld is
port(
	primary_op:			in	std_logic_vector(7 downto 0);
	secondary_op:		in	std_logic_vector(7 downto 0);
	result:				out	std_logic_vector(7 downto 0);
	secondary_result:	out	std_logic_vector(7 downto 0);
	flags_out:			out	std_logic_vector(7 downto 0)
);
end component;

component rlc8bit is
port(
	operand:	in	std_logic_vector(7 downto 0);
	output:		out	std_logic_vector(7 downto 0);
	flags_out:	out	std_logic_vector(7 downto 0)
);
end component;

component rl8bit is
port(
	operand:	in	std_logic_vector(7 downto 0);
	carry_in:	in	std_logic;
	output:		out	std_logic_vector(7 downto 0);
	flags_out:	out	std_logic_vector(7 downto 0)
);
end component;

component rrc8bit is
port(
	operand:	in	std_logic_vector(7 downto 0);
	output:		out	std_logic_vector(7 downto 0);
	flags_out:	out	std_logic_vector(7 downto 0)
);
end component;

component rr8bit is
port(
	operand:	in	std_logic_vector(7 downto 0);
	carry_in:	in	std_logic;
	output:		out	std_logic_vector(7 downto 0);
	flags_out:	out	std_logic_vector(7 downto 0)
);
end component;

component daa is
port(
	operand:	in	std_logic_vector(7 downto 0);
	flags_in:	in	std_logic_vector(7 downto 0);
	output:		out	std_logic_vector(7 downto 0);
	flags_out:	out	std_logic_vector(7 downto 0)
);
end component;

component bit_op is
port(
	operand1:	in	std_logic_vector(7 downto 0);
	operand2:	in	std_logic_vector(7 downto 0);
	flags_out:	out	std_logic_vector(7 downto 0)
);
end component;

component encoder32xN is
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
end component;

component encoder2xN_oe is
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
end component;

signal	add_result:				std_logic_vector(7 downto 0);
signal	add_carry_in:			std_logic;
signal	add_flags:				std_logic_vector(7 downto 0);

signal	and_result:				std_logic_vector(7 downto 0);
signal	and_flags:				std_logic_vector(7 downto 0);

signal	or_result:				std_logic_vector(7 downto 0);
signal	or_flags:				std_logic_vector(7 downto 0);

signal	xor_result:				std_logic_vector(7 downto 0);
signal	xor_flags:				std_logic_vector(7 downto 0);

signal	cpl_result:				std_logic_vector(7 downto 0);
signal	cpl_flags:				std_logic_vector(7 downto 0);

signal	subtract_result:		std_logic_vector(7 downto 0);
signal	subtract_borrow_in:		std_logic;
signal	subtract_flags:			std_logic_vector(7 downto 0);

signal	rlc_result:				std_logic_vector(7 downto 0);
signal	rlc_flags:				std_logic_vector(7 downto 0);

signal	rrc_result:				std_logic_vector(7 downto 0);
signal	rrc_flags:				std_logic_vector(7 downto 0);

signal	rl_result:				std_logic_vector(7 downto 0);
signal	rl_flags:				std_logic_vector(7 downto 0);

signal	rr_result:				std_logic_vector(7 downto 0);
signal	rr_flags:				std_logic_vector(7 downto 0);

signal	daa_result:				std_logic_vector(7 downto 0);
signal	daa_flags:				std_logic_vector(7 downto 0);

signal	scf_flags:				std_logic_vector(7 downto 0);

signal	ccf_carry:				std_logic;
signal	ccf_flags:				std_logic_vector(7 downto 0);

signal	bit_zero:				std_logic;
signal	bit_flags:				std_logic_vector(7 downto 0);

signal	in_flags:				std_logic_vector(7 downto 0);	-- flags for IN r, C instruction

signal	secondary_out_enable:	std_logic;						-- '1' when executing a rrd/rld
																-- instruction

signal	rld_result:				std_logic_vector(7 downto 0);
signal	secondary_rld_result:	std_logic_vector(7 downto 0);
signal	rld_flags:				std_logic_vector(7 downto 0);
signal	is_rld:					std_logic;

signal	rrd_result:				std_logic_vector(7 downto 0);
signal	secondary_rrd_result:	std_logic_vector(7 downto 0);
signal	rrd_flags:				std_logic_vector(7 downto 0);
signal	is_rrd:					std_logic;

signal	sla_result:				std_logic_vector(7 downto 0);
signal	sla_flags:				std_logic_vector(7 downto 0);

signal	sra_result:				std_logic_vector(7 downto 0);
signal	sra_flags:				std_logic_vector(7 downto 0);

signal	sll_result:				std_logic_vector(7 downto 0);
signal	sll_flags:				std_logic_vector(7 downto 0);

signal	srl_result:				std_logic_vector(7 downto 0);
signal	srl_flags:				std_logic_vector(7 downto 0);

signal	bmtc_result:			std_logic_vector(7 downto 0);
signal	bmtc_flags:				std_logic_vector(7 downto 0);	-- block move termination criterion
																-- flags

begin
	-- result multiplexer, 32x8
	u1: encoder32xN
			generic map(
			N => 8
			)
			port map(
			data0 => add_result,								-- add, ignore carry bit
			data1 => add_result,								-- add, add carry bit
			data2 => subtract_result,							-- sub, ignore borrow bit
			data3 => subtract_result, 							-- sub, subtract borrow bit
			data4 => and_result,								-- and
			data5 => xor_result,								-- xor
			data6 => or_result,									-- or
			data7 => subtract_result,							-- compare (no-borrow sub with result
																-- discarded, used to set flags)
			data8 => rlc_result,								-- RLC
			data9 => rrc_result,								-- RRC
			data10 => rl_result,								-- RL
			data11 => rr_result,								-- RR
			data12 => daa_result,								-- DAA
			data13 => cpl_result,								-- CPL
			data14 => primary_operand,							-- SCF
			data15 => primary_operand,							-- CCF
			data16 => sla_result,								-- SLA
			data17 => sra_result,								-- SRA
			data18 => sll_result,								-- SLL
			data19 => srl_result,								-- SRL
			data20 => secondary_operand,						-- BIT
			data21 => and_result,								-- RES
			data22 => or_result,								-- SET
			data23 => primary_operand,							-- IN r, (C)
			data24 => rld_result,								-- RLD
			data25 => rrd_result,								-- RRD
			data26 => bmtc_result,								-- block move termination criterion
			data27 => (others => '0'),							-- reserved
			data28 => (others => '0'),							-- reserved
			data29 => (others => '0'),							-- reserved
			data30 => (others => '0'),							-- reserved
			data31 => (others => '0'),							-- reserved
			address => operation,
			output => output
		);

	-- result flags multiplexer
	u2: encoder32xN
			generic map(
			N => 8
			)
			port map(
			data0 => add_flags,								-- add
			data1 => add_flags,								-- adc
			data2 => subtract_flags,						-- sub
			data3 => subtract_flags,						-- sbc
			data4 => and_flags,								-- and
			data5 => xor_flags,								-- xor
			data6 => or_flags,								-- or
			data7 => subtract_flags,						-- cmp
			data8 => rlc_flags,								-- rlc
			data9 => rrc_flags,								-- rrc
			data10 => rl_flags,								-- rl
			data11 => rr_flags,								-- rr
			data12 => daa_flags,							-- daa
			data13 => cpl_flags,							-- cpl
			data14 => scf_flags,							-- scf
			data15 => ccf_flags,							-- ccf
			data16 => sla_flags,							-- SLA
			data17 => sra_flags,							-- SRA
			data18 => sll_flags,							-- SLL
			data19 => srl_flags,							-- SRL
			data20 => bit_flags,							-- BIT
			data21 => (others => '0'),						-- RES, no flags affected
			data22 => (others => '0'),						-- SET, no flags affected
			data23 => in_flags,								-- IN r, (C)
			data24 => rld_flags,							-- RLD
			data25 => rrd_flags,							-- RRD
			data26 => bmtc_flags,							-- block move termination criterion
			data27 => (others => '0'),						-- reserved
			data28 => (others => '0'),						-- reserved
			data29 => (others => '0'),						-- reserved
			data30 => (others => '0'),						-- reserved
			data31 => (others => '0'),						-- reserved
			address => operation,
			output => flags_out
		);

	scf_flags <= (carry_bit => '1', others => '0');

	-- adder: This version gets flagged by ModelSim on the carry_in line as an error.  Only signals or
	-- maybe variables are allowed. Expressions are not.
--	u3: adder8x2 port map(
--			addend => primary_operand,
--			augend => secondary_operand,
--			carry_in => (flags_in(carry_bit) and operation(0)),	-- carry only with adc opcode, others
--																-- made irrelevant by result mux
--			sum => add_result,
--			carry_out => carry_out,
--			overflow => add_overflow,
--			interdigit_carry => interdigit_carry,
--			zero => add_zero
--		);

	-- adder
	u3: adder8x2 port map(
			addend => primary_operand,
			augend => secondary_operand,
			carry_in => add_carry_in,
			sum => add_result,
			flags_out => add_flags
		);

	add_carry_in <= flags_in(carry_bit) and operation(0);	-- carry only with adc opcode, others made
															-- irrelevant by result mux

	-- subtractor
	u4: subtractor8x2 port map(
			minuend => primary_operand,
			subtrahend => secondary_operand,
			borrow_in => subtract_borrow_in,
			difference => subtract_result,
			flags_out => subtract_flags
		);

	-- borrow only with sbc opcode, must remove compare opcode (operation(2 downto 0) = "111"), others
	-- made irrelevant by result mux
	subtract_borrow_in <= flags_in(carry_bit) and (not operation(2)) and operation(1) and operation(0);

	-- bitwise and operation
	u5: and8bit port map(
			operand1 => primary_operand,
			operand2 => secondary_operand,
			output => and_result,
			flags_out => and_flags
		);

	-- bitwise exclusive-or operation
	u6: xor8bit port map(
			operand1 => primary_operand,
			operand2 => secondary_operand,
			output => xor_result,
			flags_out => xor_flags
		);

	-- bitwise or operation
	u7: or8bit port map(
			operand1 => primary_operand,
			operand2 => secondary_operand,
			output => or_result,
			flags_out => or_flags
		);

	-- RLC generator
	u8:	rlc8bit port map(
			operand => primary_operand,
			output => rlc_result,
			flags_out => rlc_flags
		);

	-- RRC generator
	u9:	rrc8bit port map(
			operand => primary_operand,
			output => rrc_result,
			flags_out => rrc_flags
		);

	-- RL generator
	u10: rl8bit port map(
			operand => primary_operand,
			carry_in => flags_in(carry_bit),
			output => rl_result,
			flags_out => rl_flags
		);

	-- RR generator
	u11: rr8bit port map(
			operand => primary_operand,
			carry_in => flags_in(carry_bit),
			output => rr_result,
			flags_out => rr_flags
		);

	-- DAA
	u12: daa port map(
			operand => primary_operand,
			flags_in => flags_in,
			output => daa_result,
			flags_out => daa_flags
		);

	-- bit testing of secondary operand against mask in primary operand
	u13: bit_op port map(
		 	operand1 => primary_operand,
			operand2 => secondary_operand,
			flags_out => bit_flags
		);

	u14: rld port map(
			primary_op => primary_operand,
			secondary_op => secondary_operand,
			result => rld_result,
			secondary_result => secondary_rld_result,
			flags_out => rld_flags
		);

	u15: magnitudeN
			generic map(
				N => 5
			)
			port map(
			a => operation,
			b => rrd_operation,
			equal => is_rrd,
			lt => open,
			gt => open
		);

	u16: magnitudeN
			generic map(
				N => 5
			)
			port map(
			a => operation,
			b => rld_operation,
			equal => is_rld,
			lt => open,
			gt => open
		);

	u17: rrd port map(
			primary_op => primary_operand,
			secondary_op => secondary_operand,
			result => rrd_result,
			secondary_result => secondary_rrd_result,
			flags_out => rrd_flags
		);

	u18: encoder2xN_oe
			generic map(
			N => 8
			)
			port map(
			data0 => secondary_rld_result,
			data1 => secondary_rrd_result,
			selector => is_rrd,
			enable => secondary_out_enable,
			output => secondary_out
		);

	secondary_out_enable <= is_rrd or is_rld;

	u19: cpl port map(
			operand => primary_operand,
			output => cpl_result,
			flags_out => cpl_flags
		);

	u20: ccf_operation port map(
			flags_in => flags_in,
			flags_out => ccf_flags
		);

	u21: in_rc_flags port map(
			operand => primary_operand,
			flags_out => in_flags
		);

	u22: sla8bit port map(
			operand => primary_operand,
			output => sla_result,
			flags_out => sla_flags
		);

	u23: sra8bit port map(
			operand => primary_operand,
			output => sra_result,
			flags_out => sra_flags
		);

	u24: sll8bit port map(
			operand => primary_operand,
			output => sll_result,
			flags_out => sll_flags
		);

	u25: srl8bit port map(
			operand => primary_operand,
			output => srl_result,
			flags_out => srl_flags
		);

	u26: bmtc port map(
			operand1 => primary_operand,
			operand2 => secondary_operand,
			output => bmtc_result,
			flags_out => bmtc_flags
		);
end;
