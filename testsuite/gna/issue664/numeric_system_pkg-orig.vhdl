------------------------------------------------------------------
----                                                          ----
----  Content: Numeric System Package                         ----
----           for Stillwater KPUs                            ----
----                                                          ----
----  Author:  E. Theodore L. Omtzigt                         ----
----           theo@stillwater-sc.com                         ----
----                                                          ----
------------------------------------------------------------------
----                                                          ----
---- Copyright (C) 2017-2018                                  ----
----               E. Theodore L. Omtzigt                     ----
----               theo@stillwater-sc.com                     ----
----                                                          ----
------------------------------------------------------------------
---
---- A posit is a tapered floating point representation. 
---- To compute with posits, the regime and exponent fields 
---- need to be consolidated. This process generates a triple 
----                (sign, exponent, fraction)
---- These triples are the input values to the arithmetic units
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

--The numeric_system package defines the number system that the
--overall system will be using. We are unifying integers, floats, and posits
--by converting them to a (sign, scale, fraction) triple. 
--
--Each number system, that is, integer, float, posit, valid, will have
--a slightly different relationship between <NBITS,ES> and bit sizes
--for scale and fraction. This pkg configures the root of that configuration.

--	static constexpr size_t escale = size_t(1) << es;         // 2^es
--	static constexpr size_t range = escale * (4 * nbits - 8); // dynamic range of the posit configuration
--	static constexpr size_t half_range = range >> 1;          // position of the fixed point
--	static constexpr size_t radix_point = half_range;
--	// the upper is 1 bit bigger than the lower because maxpos^2 has that scale
--	static constexpr size_t upper_range = half_range + 1;     // size of the upper accumulator
--	static constexpr size_t qbits = range + capacity;     // size of the quire minus the sign bit: we are managing the sign explicitly

package numeric_system_pkg is

	generic(
		NBITS : positive := 8;          -- number of bits to represent encoding
		EBITS : natural  := 0;          -- number of bits to represent exponent
		SBITS : positive := 5;          -- number of bits to represent scale
		FBITS : natural  := 5;          -- number of bits to represent the fraction
		CBITS : natural  := 10          -- number of bits to represent quire capacity
	);

	-- arithmetic property constants
	constant GUARDBITS : natural := 3;

	impure function adder_width return positive;
	impure function multiplier_width return positive;
	impure function divider_width return positive;
	impure function posit_dynamic_range return positive;
	impure function quire_width return positive;

	-- derived entities
	constant BITS_ADDER_OUT      : positive := adder_width;
	constant BITS_MULTIPLIER_OUT : positive := multiplier_width;
	constant BITS_DIVIDER_OUT    : positive := divider_width;
	constant BITS_MAX_ARITH_OUT  : positive := divider_width;
	constant BITS_DYNAMIC_RANGE  : positive := posit_dynamic_range;
	constant BITS_QUIRE_WIDTH    : positive := quire_width;

	-- posits that don't have fraction bits, still have a hidden bit
	-- don't know what FBITS - 1 downto 0 evaluates to: possibly empty/null and thus trouble

	-- posit encoding is a 2's complement encoding with respect to relational operators
	subtype posit_word is signed(NBITS - 1 downto 0);
	-- scales are integers and operators on scale are add/sub
	subtype scale_word is signed(SBITS - 1 downto 0);
	-- fractions are unsigned binary values with radix at FBITS-1
	subtype fraction_word is unsigned(FBITS - 1 downto 0);

	-- ssf triple === (sign, scale, fraction) normal form
	-- ssf triples are stored in the register file
	type ssf_type is record
		isNaR    : std_logic;           -- encoding special case of Not a Real
		isZero   : std_logic;           -- encoding special case of 0
		sign     : std_logic;           -- sign
		scale    : scale_word;          -- scale of the posit: useed ^ k * 2^e
		fraction : fraction_word;
	end record;

	subtype add_significant_word is std_logic_vector(BITS_ADDER_OUT - 1 downto 0);
	-- sss triple === (sign, scale, significant) output of an adder/subtractor
	-- these records flow to the quire and the posit rounding stage
	type sss_add_output_type is record
		valid       : std_logic;
		isNaR       : std_logic;        -- encoding special case of Not a Real
		isZero      : std_logic;        -- encoding special case of 0
		sign        : std_logic;        -- sign
		scale       : scale_word;       -- scale of the posit: useed ^ k * 2^e
		significant : add_significant_word;
	end record;

	subtype mul_significant_word is std_logic_vector(BITS_MULTIPLIER_OUT - 1 downto 0);
	-- sss triple === (sign, scale, signficant) output of a multiplier
	-- these records flow to the quire and the posit rounding stage
	type sss_mul_output_type is record
		valid       : std_logic;
		isNaR       : std_logic;        -- encoding special case of Not a Real
		isZero      : std_logic;        -- encoding special case of 0
		sign        : std_logic;        -- sign
		scale       : scale_word;       -- scale of the posit: useed ^ k * 2^e
		significant : mul_significant_word;
	end record;

	subtype div_significant_word is std_logic_vector(BITS_DIVIDER_OUT - 1 downto 0);
	-- sss triple === (sign, scale, signficant) output of a divider
	-- these records flow to the quire and the posit rounding stage
	type sss_div_output_type is record
		isNaR       : std_logic;        -- encoding special case of Not a Real
		isZero      : std_logic;        -- encoding special case of 0
		sign        : std_logic;        -- sign
		scale       : scale_word;       -- scale of the posit: useed ^ k * 2^e
		significant : div_significant_word;
	end record;

	-- max sss triple that can come out of the arithmetic data path.
	-- all smaller sss triples are right extended with '0's to unify
	type sss_max_result_type is record
		isNaR       : std_logic;        -- encoding special case of Not a Real
		isZero      : std_logic;        -- encoding special case of 0
		sign        : std_logic;        -- sign
		scale       : scale_word;       -- scale of the posit: useed ^ k * 2^e
		significant : std_logic_vector(BITS_MAX_ARITH_OUT - 1 downto 0);
	end record;

	-- input record to the ALU
	type alu_input_type is record
		opcode    : std_logic_vector(2 downto 0); -- instruction to execute: add/sub, mul/div, recip, sqrt
		operand_1 : ssf_type;
		operand_2 : ssf_type;
		operand_3 : ssf_type;
	end record;

	type quire_cmd_type is (LOAD, STORE, CLEAR, NOP); -- ACCUMULATE is default: keeps to 2 bits

	-- are we managing the quire as a sign-magnitude or a 2's complement accumulator?
	-- subtype quire_word is unsigned(BITS_QUIRE_WIDTH - 1 downto 0);
	subtype quire_word is signed(BITS_QUIRE_WIDTH - 1 downto 0);
	constant QUIRE_ZERO : quire_word := (others => '0');

	type quire_in_type is record
		cmd  : quire_cmd_type;
		d_in : quire_word;              -- for quire load
	end record;

	subtype quire_in_reg is ssf_type;   -- adding a raw posit value out of the posit registers
	subtype quire_in_add is sss_add_output_type; -- adding the result of a posit addition without rounding
	subtype quire_in_mul is sss_mul_output_type; -- adding the result of a posit multiplication without rounding

	type quire_state_type is (QS_ZERO, QS_NEG, QS_POS, QS_OVERFLOW);

	type quire_out_type is record
		state : quire_state_type;
		q_out : quire_word;             -- for quire store
	end record;

	-- stringer for ssf tripslet
	function ssf_to_string(
		tag     : String;
		triplet : ssf_type
	) return String;

	-- compare two ssf triplets
	function cmpSSF(
		val : ssf_type;
		ref : ssf_type
	) return boolean;

	-- report the configuration and the derived constants
	procedure reportConfiguration;

end package;

package body numeric_system_pkg is

	impure function adder_width return positive is
		constant fbits_value : positive := FBITS;
	begin
		return fbits_value + 2;
	end function;

	impure function multiplier_width return positive is
		constant fbits_value : positive := FBITS;
	begin
		return 2 * fbits_value + 1;
	end function;

	impure function divider_width return positive is
		constant fbits_value : positive := FBITS;
	begin
		return 2 * fbits_value + 3;
	end function;

	impure function posit_dynamic_range return positive is
		constant ebits_value : natural  := EBITS;
		constant nbits_value : positive := NBITS;
		variable escale      : positive := 1;
	begin
		escale := 2 ** ebits_value;
		return escale * (4 * nbits_value - 8);
	end function;

	impure function quire_width return positive is
		constant ebits_value : natural  := EBITS;
		constant nbits_value : positive := NBITS;
		constant cbits_value : positive := CBITS;
		variable escale      : positive := 1;
		variable qw          : positive := 1;
	begin
		escale := 2 ** ebits_value;
		qw     := escale * (4 * nbits_value - 8) + cbits_value;
		return qw;
	end function;

	procedure reportConfiguration is
	begin
		report "Configuration Parameters";
		report "  NBITS               = " & to_string(NBITS);
		report "  EBITS               = " & to_string(EBITS);
		report "  SBITS               = " & to_string(SBITS);
		report "  FBITS               = " & to_string(FBITS);
		report "  CBITS               = " & to_string(CBITS);

		report "  GUARDBITS           = " & to_string(GUARDBITS);

		report "Derived Constants";
		report "  BITS_ADDER_OUT      : " & to_string(BITS_ADDER_OUT);
		report "  BITS_MULTIPLIER_OUT : " & to_string(BITS_MULTIPLIER_OUT);
		report "  BITS_DIVIDER_OUT    : " & to_string(BITS_DIVIDER_OUT);
		report "  BITS_MAX_ARITH_OUT  : " & to_string(BITS_MAX_ARITH_OUT);
		report "  BITS_DYNAMIC_RANGE  : " & to_string(BITS_DYNAMIC_RANGE);
		report "  BITS_QUIRE_WIDTH    : " & to_string(BITS_QUIRE_WIDTH);
	end procedure;

	function ssf_to_string(
		tag     : String;
		triplet : ssf_type
	) return String is
	begin
		if triplet.isZero = '1' then
			return tag & " = (zero)";
		elsif triplet.isNaR = '1' then
			return tag & " = (NaR)";
		else
			return tag & " = (" & to_string(triplet.sign) & "," & to_string(triplet.scale) & "," & to_string(triplet.fraction) & ")";
		end if;
	end function;

	function cmpSSF(
		val : ssf_type;
		ref : ssf_type
	) return boolean is
	begin
		-- first check special cases
		if val.isZero = '1' AND val.isZero = ref.isZero then
			return true;
		end if;
		if val.isNaR = '1' AND val.isNaR = ref.isNaR then
			return true;
		end if;
		-- no special case, so compare ssf values
		if val.sign = ref.sign AND val.scale = ref.scale AND val.fraction = ref.fraction then
			return true;
		else
			return false;
		end if;

	end function;

end package body numeric_system_pkg;

------------------------------------------------------------------
---                    posit configuration                     ---
------------------------------------------------------------------
-- configuration for a posit<5,1>@1.0   : [s] [rr] [e] [f] [ggg]
-- es = 1 -> useed = 2^2^1 = 4
-- max k = nbits-2 = 3 -> 4^3 = 2^6 -> maxpos = 64
-- fraction bits [h] [f] [ggg]  -> 1 hidden bit, 1 fraction bit, 3 guard bits  == 5 bits total
-- adder out: 5 + 1 = 6

-- configuration for a posit<8,0>@1.0 : [s] [rr] [] [fffff] [ggg]
-- es = 0 -> useed = 2^2^0 = 2
-- max k = nbits-2 = 6 -> 2^6      -> maxpos = 64 -> scale is 6+1 bits
-- fraction bits [h] [ffff] [ggg]  -> 1 hidden bit, 4 fraction bit, 3 guard bits  == 8 bits total
-- adder out: 8 + 1 = 9

-- configuration for a posit<8,1>@1.0 : [s] [rr] [e] [ffff] [ggg]
-- es = 1 -> useed = 2^2^1 = 4
-- max k = nbits-2 = 6 -> 4^6 = 2^12 -> maxpos = 4096 -> scale is 12+1 bits
-- fraction bits [h] [ffff] [ggg]  -> 1 hidden bit, 4 fraction bit, 3 guard bits  == 8 bits total
-- adder out: 8 + 1 = 9

------------------------------------------------------------------
---                    float configuration                     ---
------------------------------------------------------------------
-- configuation for a 8-bit float      : [s] [ee] [f ffff] [ggg]
-- nbits = 8, es = 2
-- configuation for a 16-bit float     : [s] [eeee e] [ff ffff ffff] [ggg]
-- nbits = 16, es = 5
-- configuation for a 32-bit float     : [s] [eeee eeee] [fff ffff ffff ffff ffff ffff] [ggg]
-- nbits = 32, es = 8

-- Standard posit configurations
-- posit<  8,0> useed scale     1     minpos scale         -6     maxpos scale          6
-- posit< 16,1> useed scale     2     minpos scale        -28     maxpos scale         28
-- posit< 32,2> useed scale     4     minpos scale       -120     maxpos scale        120
-- posit< 64,3> useed scale     8     minpos scale       -496     maxpos scale        496

package p4e0 is new work.numeric_system_pkg
	generic map(
		NBITS => 4,                     -- 4 encoding bits
		EBITS => 0,                     -- no exponent bits
		SBITS => 3,                     -- posit<4,0> scale ranges from -2 to 2
		FBITS => 1,                     -- one fraction bit
		CBITS => 10                     -- 10 capacity bits for the quire		
	);

package p5e0 is new work.numeric_system_pkg
	generic map(
		NBITS => 5,                     -- 5 encoding bits
		EBITS => 0,                     -- no exponent bits
		SBITS => 4,                     -- posit<5,0> scale ranges from -3 to 3
		FBITS => 2,                     -- 2 fraction bits
		CBITS => 10                     -- 10 capacity bits for the quire
	);

package p8e0 is new work.numeric_system_pkg
	generic map(
		NBITS => 8,                     -- 8 encoding bits
		EBITS => 0,                     -- no exponent bits
		SBITS => 5,                     -- posit<8,0> scale ranges from -6 to 6
		FBITS => 5,                     -- 5 fraction bits
		CBITS => 10                     -- 10 capacity bits for the quire
	);

package p8e1 is new work.numeric_system_pkg
	generic map(
		NBITS => 8,                     -- 8 encoding bits
		EBITS => 1,                     -- 1 exponent bit
		SBITS => 6,                     -- posit<8,1> scale ranges from -12 to 12
		FBITS => 4,                     -- 4 fraction bits
		CBITS => 10                     -- 10 capacity bits for the quire
	);

package p8e2 is new work.numeric_system_pkg
	generic map(
		NBITS => 8,                     -- 8 encoding bits
		EBITS => 2,                     -- 2 exponent bits
		SBITS => 7,                     -- posit<8,2> scale ranges from -24 to 24
		FBITS => 3,                     -- 3 fraction bits
		CBITS => 10                     -- 10 capacity bits for the quire
	);

package p16e1 is new work.numeric_system_pkg
	generic map(
		NBITS => 16,                    -- 16 encoding bits
		EBITS => 1,                     -- one exponent bit
		SBITS => 7,                     -- posit<16,1> scale ranges from -28 to 28
		FBITS => 12,                    -- 12 fraction bits
		CBITS => 10                     -- 10 capacity bits for the quire
	);

-- posit<  8,0> useed scale     1     minpos scale         -6     maxpos scale          6
-- posit<  8,1> useed scale     2     minpos scale        -12     maxpos scale         12
-- posit<  8,2> useed scale     4     minpos scale        -24     maxpos scale         24
-- posit< 16,0> useed scale     1     minpos scale        -14     maxpos scale         14
-- posit< 16,1> useed scale     2     minpos scale        -28     maxpos scale         28
-- posit< 16,2> useed scale     4     minpos scale        -56     maxpos scale         56

------------------------------------------------------------------
----                                                          ----
----  Content: Data Path Configuration Package                ----
----           for Stillwater KPUs                            ----
----                                                          ----
----  Author:  E. Theodore L. Omtzigt                         ----
----           theo@stillwater-sc.com                         ----
----                                                          ----
------------------------------------------------------------------
----                                                          ----
---- Copyright (C) 2017-2018                                  ----
----               E. Theodore L. Omtzigt                     ----
----               theo@stillwater-sc.com                     ----
----                                                          ----
------------------------------------------------------------------

-- generic configuration package name that we can use in the components
-- and can change here to create different configurations
package config_pkg is new work.numeric_system_pkg
	generic map(
		NBITS => 8,                     -- 8 encoding bits
		EBITS => 1,                     -- no exponent bits
		SBITS => 5,                     -- posit<8,0> scale ranges from -6 to 6
		FBITS => 5,                     -- 5 fraction bits
		CBITS => 8                      -- 8 capacity bits for quire
	);

