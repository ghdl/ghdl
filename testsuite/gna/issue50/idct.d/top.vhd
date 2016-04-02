library ieee;
use ieee.std_logic_1164.all;

entity top is
	port (
		clock : in  std_logic;
		reset : in  std_logic;
		start : in  std_logic;
		cp_ok : out std_logic;
		cp_en : in  std_logic;
		cp_rest : in  std_logic;
		cp_din : in  std_logic_vector(63 downto 0);
		cp_dout : out std_logic_vector(63 downto 0);
		stdout_data : out std_logic_vector(7 downto 0);
		stdout_rdy : out std_logic;
		stdout_ack : in  std_logic;
		stdin_data : in  std_logic_vector(31 downto 0);
		stdin_rdy : out std_logic;
		stdin_ack : in  std_logic
	);
end top;

architecture augh of top is

	-- Declaration of components

	component output_split2 is
		port (
			wa0_data : in  std_logic_vector(7 downto 0);
			wa0_addr : in  std_logic_vector(2 downto 0);
			ra0_data : out std_logic_vector(7 downto 0);
			ra0_addr : in  std_logic_vector(2 downto 0);
			wa0_en : in  std_logic;
			clk : in  std_logic
		);
	end component;

	component output_split3 is
		port (
			wa0_data : in  std_logic_vector(7 downto 0);
			wa0_addr : in  std_logic_vector(2 downto 0);
			ra0_data : out std_logic_vector(7 downto 0);
			ra0_addr : in  std_logic_vector(2 downto 0);
			wa0_en : in  std_logic;
			clk : in  std_logic
		);
	end component;

	component sub_159 is
		port (
			gt : out std_logic;
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0);
			sign : in  std_logic
		);
	end component;

	component add_165 is
		port (
			result : out std_logic_vector(26 downto 0);
			in_a : in  std_logic_vector(26 downto 0);
			in_b : in  std_logic_vector(26 downto 0)
		);
	end component;

	component output_split1 is
		port (
			wa0_data : in  std_logic_vector(7 downto 0);
			wa0_addr : in  std_logic_vector(2 downto 0);
			ra0_data : out std_logic_vector(7 downto 0);
			ra0_addr : in  std_logic_vector(2 downto 0);
			wa0_en : in  std_logic;
			clk : in  std_logic
		);
	end component;

	component output_split0 is
		port (
			wa0_data : in  std_logic_vector(7 downto 0);
			wa0_addr : in  std_logic_vector(2 downto 0);
			ra0_data : out std_logic_vector(7 downto 0);
			ra0_addr : in  std_logic_vector(2 downto 0);
			wa0_en : in  std_logic;
			clk : in  std_logic
		);
	end component;

	component add_172 is
		port (
			result : out std_logic_vector(19 downto 0);
			in_a : in  std_logic_vector(19 downto 0);
			in_b : in  std_logic_vector(19 downto 0)
		);
	end component;

	component add_176 is
		port (
			result : out std_logic_vector(19 downto 0);
			in_a : in  std_logic_vector(19 downto 0);
			in_b : in  std_logic_vector(19 downto 0)
		);
	end component;

	component add_181 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component sub_187 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component mul_189 is
		port (
			result : out std_logic_vector(30 downto 0);
			in_a : in  std_logic_vector(30 downto 0);
			in_b : in  std_logic_vector(13 downto 0)
		);
	end component;

	component add_191 is
		port (
			result : out std_logic_vector(30 downto 0);
			in_a : in  std_logic_vector(30 downto 0);
			in_b : in  std_logic_vector(30 downto 0)
		);
	end component;

	component mul_192 is
		port (
			result : out std_logic_vector(29 downto 0);
			in_a : in  std_logic_vector(29 downto 0);
			in_b : in  std_logic_vector(10 downto 0)
		);
	end component;

	component mul_193 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(14 downto 0)
		);
	end component;

	component mul_198 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(14 downto 0)
		);
	end component;

	component mul_199 is
		port (
			result : out std_logic_vector(30 downto 0);
			in_a : in  std_logic_vector(30 downto 0);
			in_b : in  std_logic_vector(13 downto 0)
		);
	end component;

	component sub_209 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component add_212 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component sub_213 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component sub_214 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component mul_215 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(15 downto 0)
		);
	end component;

	component mul_216 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(14 downto 0)
		);
	end component;

	component sub_217 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component mul_218 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(14 downto 0)
		);
	end component;

	component mul_219 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(15 downto 0)
		);
	end component;

	component sub_220 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component mul_223 is
		port (
			result : out std_logic_vector(30 downto 0);
			in_a : in  std_logic_vector(30 downto 0);
			in_b : in  std_logic_vector(14 downto 0)
		);
	end component;

	component sub_227 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component sub_157 is
		port (
			ge : out std_logic;
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0);
			sign : in  std_logic
		);
	end component;

	component add_163 is
		port (
			result : out std_logic_vector(15 downto 0);
			in_a : in  std_logic_vector(15 downto 0);
			in_b : in  std_logic_vector(15 downto 0)
		);
	end component;

	component cmp_164 is
		port (
			ne : out std_logic;
			in0 : in  std_logic_vector(15 downto 0);
			in1 : in  std_logic_vector(15 downto 0)
		);
	end component;

	component add_170 is
		port (
			result : out std_logic_vector(19 downto 0);
			in_a : in  std_logic_vector(19 downto 0);
			in_b : in  std_logic_vector(19 downto 0)
		);
	end component;

	component add_174 is
		port (
			result : out std_logic_vector(19 downto 0);
			in_a : in  std_logic_vector(19 downto 0);
			in_b : in  std_logic_vector(19 downto 0)
		);
	end component;

	component add_180 is
		port (
			result : out std_logic_vector(19 downto 0);
			in_a : in  std_logic_vector(19 downto 0);
			in_b : in  std_logic_vector(19 downto 0)
		);
	end component;

	component sub_186 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component mul_190 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(14 downto 0)
		);
	end component;

	component mul_196 is
		port (
			result : out std_logic_vector(29 downto 0);
			in_a : in  std_logic_vector(29 downto 0);
			in_b : in  std_logic_vector(10 downto 0)
		);
	end component;

	component sub_200 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component add_206 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component add_210 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component add_171 is
		port (
			result : out std_logic_vector(26 downto 0);
			in_a : in  std_logic_vector(26 downto 0);
			in_b : in  std_logic_vector(26 downto 0)
		);
	end component;

	component add_177 is
		port (
			result : out std_logic_vector(26 downto 0);
			in_a : in  std_logic_vector(26 downto 0);
			in_b : in  std_logic_vector(26 downto 0)
		);
	end component;

	component add_179 is
		port (
			result : out std_logic_vector(26 downto 0);
			in_a : in  std_logic_vector(26 downto 0);
			in_b : in  std_logic_vector(26 downto 0)
		);
	end component;

	component mul_195 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(14 downto 0)
		);
	end component;

	component sub_197 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component sub_207 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component mul_230 is
		port (
			result : out std_logic_vector(30 downto 0);
			in_a : in  std_logic_vector(30 downto 0);
			in_b : in  std_logic_vector(14 downto 0)
		);
	end component;

	component sub_185 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component add_211 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component add_226 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component add_235 is
		port (
			result : out std_logic_vector(26 downto 0);
			in_a : in  std_logic_vector(26 downto 0);
			in_b : in  std_logic_vector(26 downto 0)
		);
	end component;

	component add_314 is
		port (
			result : out std_logic_vector(26 downto 0);
			in_a : in  std_logic_vector(26 downto 0);
			in_b : in  std_logic_vector(26 downto 0)
		);
	end component;

	component sub_160 is
		port (
			le : out std_logic;
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0);
			sign : in  std_logic
		);
	end component;

	component add_173 is
		port (
			result : out std_logic_vector(26 downto 0);
			in_a : in  std_logic_vector(26 downto 0);
			in_b : in  std_logic_vector(26 downto 0)
		);
	end component;

	component add_182 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component sub_188 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component sub_243 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component sub_262 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component output_split4 is
		port (
			wa0_data : in  std_logic_vector(7 downto 0);
			wa0_addr : in  std_logic_vector(2 downto 0);
			ra0_data : out std_logic_vector(7 downto 0);
			ra0_addr : in  std_logic_vector(2 downto 0);
			wa0_en : in  std_logic;
			clk : in  std_logic
		);
	end component;

	component output_split5 is
		port (
			wa0_data : in  std_logic_vector(7 downto 0);
			wa0_addr : in  std_logic_vector(2 downto 0);
			ra0_data : out std_logic_vector(7 downto 0);
			ra0_addr : in  std_logic_vector(2 downto 0);
			wa0_en : in  std_logic;
			clk : in  std_logic
		);
	end component;

	component output_split6 is
		port (
			wa0_data : in  std_logic_vector(7 downto 0);
			wa0_addr : in  std_logic_vector(2 downto 0);
			ra0_data : out std_logic_vector(7 downto 0);
			ra0_addr : in  std_logic_vector(2 downto 0);
			wa0_en : in  std_logic;
			clk : in  std_logic
		);
	end component;

	component output_split7 is
		port (
			wa0_data : in  std_logic_vector(7 downto 0);
			wa0_addr : in  std_logic_vector(2 downto 0);
			ra0_data : out std_logic_vector(7 downto 0);
			ra0_addr : in  std_logic_vector(2 downto 0);
			wa0_en : in  std_logic;
			clk : in  std_logic
		);
	end component;

	component input_split0 is
		port (
			ra0_data : out std_logic_vector(31 downto 0);
			ra0_addr : in  std_logic_vector(4 downto 0);
			ra1_data : out std_logic_vector(31 downto 0);
			ra1_addr : in  std_logic_vector(4 downto 0);
			ra2_data : out std_logic_vector(31 downto 0);
			ra2_addr : in  std_logic_vector(4 downto 0);
			ra3_data : out std_logic_vector(31 downto 0);
			ra3_addr : in  std_logic_vector(4 downto 0);
			clk : in  std_logic;
			wa2_data : in  std_logic_vector(31 downto 0);
			wa2_addr : in  std_logic_vector(4 downto 0);
			wa2_en : in  std_logic
		);
	end component;

	component add_194 is
		port (
			result : out std_logic_vector(29 downto 0);
			in_a : in  std_logic_vector(29 downto 0);
			in_b : in  std_logic_vector(29 downto 0)
		);
	end component;

	component add_205 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component add_254 is
		port (
			result : out std_logic_vector(26 downto 0);
			in_a : in  std_logic_vector(26 downto 0);
			in_b : in  std_logic_vector(26 downto 0)
		);
	end component;

	component add_276 is
		port (
			result : out std_logic_vector(26 downto 0);
			in_a : in  std_logic_vector(26 downto 0);
			in_b : in  std_logic_vector(26 downto 0)
		);
	end component;

	component sub_284 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component input_split1 is
		port (
			wa0_data : in  std_logic_vector(31 downto 0);
			wa0_addr : in  std_logic_vector(4 downto 0);
			ra0_data : out std_logic_vector(31 downto 0);
			ra0_addr : in  std_logic_vector(4 downto 0);
			wa0_en : in  std_logic;
			ra1_data : out std_logic_vector(31 downto 0);
			ra1_addr : in  std_logic_vector(4 downto 0);
			ra2_data : out std_logic_vector(31 downto 0);
			ra2_addr : in  std_logic_vector(4 downto 0);
			ra3_data : out std_logic_vector(31 downto 0);
			ra3_addr : in  std_logic_vector(4 downto 0);
			clk : in  std_logic
		);
	end component;

	component add_166 is
		port (
			result : out std_logic_vector(19 downto 0);
			in_a : in  std_logic_vector(19 downto 0);
			in_b : in  std_logic_vector(19 downto 0)
		);
	end component;

	component add_168 is
		port (
			result : out std_logic_vector(19 downto 0);
			in_a : in  std_logic_vector(19 downto 0);
			in_b : in  std_logic_vector(19 downto 0)
		);
	end component;

	component add_178 is
		port (
			result : out std_logic_vector(19 downto 0);
			in_a : in  std_logic_vector(19 downto 0);
			in_b : in  std_logic_vector(19 downto 0)
		);
	end component;

	component add_183 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component sub_332 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component mul_341 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(15 downto 0)
		);
	end component;

	component mul_357 is
		port (
			result : out std_logic_vector(30 downto 0);
			in_a : in  std_logic_vector(30 downto 0);
			in_b : in  std_logic_vector(14 downto 0)
		);
	end component;

	component mul_365 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(14 downto 0)
		);
	end component;

	component mul_368 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(15 downto 0)
		);
	end component;

	component sub_369 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component sub_370 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component sub_377 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component cmp_398 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component cmp_400 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component cmp_404 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component cmp_406 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component cmp_408 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component cmp_410 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component cmp_412 is
		port (
			eq : out std_logic;
			in0 : in  std_logic;
			in1 : in  std_logic
		);
	end component;

	component sub_429 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component add_466 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component sub_496 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component sub_521 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component sub_528 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component fsm_23 is
		port (
			clock : in  std_logic;
			reset : in  std_logic;
			in0 : in  std_logic;
			out181 : out std_logic;
			out182 : out std_logic;
			out183 : out std_logic;
			out184 : out std_logic;
			out185 : out std_logic;
			out8 : out std_logic;
			out13 : out std_logic;
			out14 : out std_logic;
			out16 : out std_logic;
			out18 : out std_logic;
			out19 : out std_logic;
			out20 : out std_logic;
			out21 : out std_logic;
			out22 : out std_logic;
			in2 : in  std_logic;
			out23 : out std_logic;
			out24 : out std_logic;
			out25 : out std_logic;
			out26 : out std_logic;
			out27 : out std_logic;
			out28 : out std_logic;
			out29 : out std_logic;
			out30 : out std_logic;
			out31 : out std_logic;
			out33 : out std_logic;
			out35 : out std_logic;
			out36 : out std_logic;
			out38 : out std_logic;
			out40 : out std_logic;
			out42 : out std_logic;
			in3 : in  std_logic;
			out44 : out std_logic;
			out46 : out std_logic;
			out48 : out std_logic;
			out49 : out std_logic;
			out50 : out std_logic;
			out52 : out std_logic;
			out54 : out std_logic;
			out56 : out std_logic;
			out57 : out std_logic;
			out58 : out std_logic;
			in4 : in  std_logic;
			out60 : out std_logic;
			in5 : in  std_logic;
			out164 : out std_logic;
			out165 : out std_logic;
			out167 : out std_logic;
			out168 : out std_logic;
			out170 : out std_logic;
			out171 : out std_logic;
			out173 : out std_logic;
			out174 : out std_logic;
			out176 : out std_logic;
			out178 : out std_logic;
			out0 : out std_logic;
			out1 : out std_logic;
			out2 : out std_logic;
			in1 : in  std_logic;
			out4 : out std_logic;
			out90 : out std_logic;
			out91 : out std_logic;
			out97 : out std_logic;
			out99 : out std_logic;
			out101 : out std_logic;
			in6 : in  std_logic;
			out103 : out std_logic;
			out105 : out std_logic;
			out106 : out std_logic;
			out107 : out std_logic;
			out108 : out std_logic;
			out135 : out std_logic;
			out136 : out std_logic;
			out137 : out std_logic;
			out138 : out std_logic;
			in11 : in  std_logic;
			out140 : out std_logic;
			out141 : out std_logic;
			out142 : out std_logic;
			out143 : out std_logic;
			out145 : out std_logic;
			out146 : out std_logic;
			out148 : out std_logic;
			out150 : out std_logic;
			out153 : out std_logic;
			out154 : out std_logic;
			out155 : out std_logic;
			out156 : out std_logic;
			out157 : out std_logic;
			out158 : out std_logic;
			out159 : out std_logic;
			out160 : out std_logic;
			out161 : out std_logic;
			out162 : out std_logic;
			out111 : out std_logic;
			out112 : out std_logic;
			out114 : out std_logic;
			out116 : out std_logic;
			out118 : out std_logic;
			out120 : out std_logic;
			out121 : out std_logic;
			out122 : out std_logic;
			out123 : out std_logic;
			out124 : out std_logic;
			out125 : out std_logic;
			out126 : out std_logic;
			in7 : in  std_logic;
			out129 : out std_logic;
			out130 : out std_logic;
			in8 : in  std_logic;
			out131 : out std_logic;
			in9 : in  std_logic;
			out132 : out std_logic;
			out133 : out std_logic;
			out134 : out std_logic;
			in10 : in  std_logic;
			out186 : out std_logic;
			out187 : out std_logic;
			out190 : out std_logic;
			out195 : out std_logic;
			out197 : out std_logic;
			out198 : out std_logic;
			out199 : out std_logic;
			out200 : out std_logic;
			out201 : out std_logic;
			out203 : out std_logic;
			out204 : out std_logic;
			out206 : out std_logic;
			out207 : out std_logic;
			out209 : out std_logic;
			out210 : out std_logic;
			out212 : out std_logic;
			out213 : out std_logic;
			out215 : out std_logic;
			out217 : out std_logic;
			out220 : out std_logic;
			out221 : out std_logic;
			out222 : out std_logic;
			out223 : out std_logic;
			out224 : out std_logic;
			out225 : out std_logic;
			out226 : out std_logic;
			out227 : out std_logic;
			out228 : out std_logic;
			out229 : out std_logic;
			out231 : out std_logic;
			out232 : out std_logic;
			out234 : out std_logic;
			out235 : out std_logic;
			out237 : out std_logic;
			out238 : out std_logic;
			out240 : out std_logic;
			out241 : out std_logic;
			out243 : out std_logic;
			out245 : out std_logic;
			out248 : out std_logic;
			out249 : out std_logic;
			out250 : out std_logic;
			out251 : out std_logic;
			out252 : out std_logic;
			out253 : out std_logic;
			out254 : out std_logic;
			out255 : out std_logic;
			out256 : out std_logic;
			out257 : out std_logic;
			out259 : out std_logic;
			out260 : out std_logic;
			out262 : out std_logic;
			out263 : out std_logic;
			out265 : out std_logic;
			out266 : out std_logic;
			out268 : out std_logic;
			out269 : out std_logic;
			out271 : out std_logic;
			out273 : out std_logic;
			out276 : out std_logic;
			out277 : out std_logic;
			out278 : out std_logic;
			out279 : out std_logic;
			out280 : out std_logic;
			out281 : out std_logic;
			out282 : out std_logic;
			out283 : out std_logic;
			out284 : out std_logic;
			out285 : out std_logic;
			out286 : out std_logic;
			out287 : out std_logic;
			out288 : out std_logic;
			out289 : out std_logic;
			out290 : out std_logic;
			out291 : out std_logic;
			out292 : out std_logic;
			out293 : out std_logic;
			out294 : out std_logic;
			out295 : out std_logic;
			out296 : out std_logic;
			out297 : out std_logic;
			out298 : out std_logic;
			out311 : out std_logic;
			out312 : out std_logic;
			out313 : out std_logic;
			out314 : out std_logic;
			out315 : out std_logic;
			out316 : out std_logic;
			out318 : out std_logic;
			out321 : out std_logic;
			out322 : out std_logic;
			out323 : out std_logic;
			out324 : out std_logic;
			out325 : out std_logic;
			out326 : out std_logic;
			out327 : out std_logic;
			out328 : out std_logic;
			out329 : out std_logic;
			out333 : out std_logic;
			out341 : out std_logic;
			out342 : out std_logic;
			out343 : out std_logic;
			out344 : out std_logic;
			out345 : out std_logic;
			out346 : out std_logic;
			out349 : out std_logic;
			out350 : out std_logic;
			out351 : out std_logic;
			out352 : out std_logic;
			out353 : out std_logic;
			out354 : out std_logic;
			out355 : out std_logic;
			out357 : out std_logic;
			out361 : out std_logic;
			out362 : out std_logic;
			out363 : out std_logic;
			out364 : out std_logic;
			out366 : out std_logic;
			out367 : out std_logic;
			out371 : out std_logic;
			out372 : out std_logic;
			out373 : out std_logic;
			out382 : out std_logic;
			out383 : out std_logic;
			out385 : out std_logic;
			out393 : out std_logic;
			out394 : out std_logic;
			out395 : out std_logic;
			out396 : out std_logic;
			out398 : out std_logic;
			out400 : out std_logic;
			out401 : out std_logic;
			out402 : out std_logic;
			out404 : out std_logic;
			out406 : out std_logic;
			out407 : out std_logic;
			out408 : out std_logic;
			out409 : out std_logic;
			out410 : out std_logic;
			out411 : out std_logic;
			out412 : out std_logic;
			out413 : out std_logic;
			out414 : out std_logic;
			out416 : out std_logic;
			out417 : out std_logic;
			out418 : out std_logic;
			out419 : out std_logic;
			out422 : out std_logic;
			out423 : out std_logic;
			out425 : out std_logic;
			out426 : out std_logic;
			out428 : out std_logic;
			out429 : out std_logic;
			out430 : out std_logic;
			out431 : out std_logic;
			out433 : out std_logic;
			out434 : out std_logic;
			out435 : out std_logic;
			out436 : out std_logic;
			out437 : out std_logic;
			out438 : out std_logic;
			out440 : out std_logic;
			out441 : out std_logic;
			out443 : out std_logic;
			out444 : out std_logic;
			out445 : out std_logic;
			out446 : out std_logic;
			out447 : out std_logic;
			out450 : out std_logic;
			out451 : out std_logic;
			out454 : out std_logic;
			out455 : out std_logic;
			out457 : out std_logic;
			out458 : out std_logic;
			out459 : out std_logic;
			out460 : out std_logic;
			out461 : out std_logic;
			out462 : out std_logic;
			out463 : out std_logic;
			out464 : out std_logic;
			out465 : out std_logic;
			out466 : out std_logic;
			out467 : out std_logic;
			out468 : out std_logic;
			out469 : out std_logic;
			out472 : out std_logic;
			out475 : out std_logic;
			out481 : out std_logic;
			out482 : out std_logic;
			out483 : out std_logic;
			out484 : out std_logic;
			out487 : out std_logic;
			out488 : out std_logic;
			out491 : out std_logic;
			out495 : out std_logic;
			out496 : out std_logic;
			out497 : out std_logic;
			out498 : out std_logic;
			out499 : out std_logic;
			out500 : out std_logic;
			out501 : out std_logic;
			out512 : out std_logic;
			out513 : out std_logic;
			out517 : out std_logic;
			out518 : out std_logic;
			out519 : out std_logic;
			out521 : out std_logic;
			out522 : out std_logic;
			out524 : out std_logic;
			out525 : out std_logic;
			out526 : out std_logic;
			out527 : out std_logic;
			out528 : out std_logic;
			out531 : out std_logic;
			out540 : out std_logic;
			out542 : out std_logic;
			out544 : out std_logic;
			out545 : out std_logic;
			out554 : out std_logic;
			out555 : out std_logic;
			out559 : out std_logic;
			out560 : out std_logic;
			out561 : out std_logic;
			out562 : out std_logic;
			out563 : out std_logic;
			out566 : out std_logic;
			out567 : out std_logic;
			out570 : out std_logic;
			out572 : out std_logic;
			out575 : out std_logic;
			out577 : out std_logic;
			out578 : out std_logic;
			out580 : out std_logic;
			out581 : out std_logic
		);
	end component;

	component add_167 is
		port (
			result : out std_logic_vector(26 downto 0);
			in_a : in  std_logic_vector(26 downto 0);
			in_b : in  std_logic_vector(26 downto 0)
		);
	end component;

	component add_169 is
		port (
			result : out std_logic_vector(26 downto 0);
			in_a : in  std_logic_vector(26 downto 0);
			in_b : in  std_logic_vector(26 downto 0)
		);
	end component;

	component add_175 is
		port (
			result : out std_logic_vector(26 downto 0);
			in_a : in  std_logic_vector(26 downto 0);
			in_b : in  std_logic_vector(26 downto 0)
		);
	end component;

	component add_255 is
		port (
			result : out std_logic_vector(19 downto 0);
			in_a : in  std_logic_vector(19 downto 0);
			in_b : in  std_logic_vector(19 downto 0)
		);
	end component;

	component sub_362 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component mul_376 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(14 downto 0)
		);
	end component;

	component add_420 is
		port (
			result : out std_logic_vector(19 downto 0);
			in_a : in  std_logic_vector(19 downto 0);
			in_b : in  std_logic_vector(19 downto 0)
		);
	end component;

	component sub_446 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component mul_456 is
		port (
			result : out std_logic_vector(30 downto 0);
			in_a : in  std_logic_vector(30 downto 0);
			in_b : in  std_logic_vector(14 downto 0)
		);
	end component;

	component mul_457 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(15 downto 0)
		);
	end component;

	component sub_461 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component sub_517 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component mul_560 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(14 downto 0)
		);
	end component;

	component mul_565 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(15 downto 0)
		);
	end component;

	component mul_578 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(14 downto 0)
		);
	end component;

	component muxb_162 is
		port (
			in_sel : in  std_logic;
			out_data : out std_logic;
			in_data0 : in  std_logic;
			in_data1 : in  std_logic
		);
	end component;

	component add_184 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component muxb_201 is
		port (
			in_sel : in  std_logic;
			out_data : out std_logic;
			in_data0 : in  std_logic;
			in_data1 : in  std_logic
		);
	end component;

	component cmp_202 is
		port (
			ne : out std_logic;
			in0 : in  std_logic_vector(15 downto 0);
			in1 : in  std_logic_vector(15 downto 0)
		);
	end component;

	component cmp_203 is
		port (
			eq : out std_logic;
			in0 : in  std_logic;
			in1 : in  std_logic
		);
	end component;

	component cmp_204 is
		port (
			eq : out std_logic;
			in0 : in  std_logic;
			in1 : in  std_logic
		);
	end component;

	component sub_208 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component add_236 is
		port (
			result : out std_logic_vector(19 downto 0);
			in_a : in  std_logic_vector(19 downto 0);
			in_b : in  std_logic_vector(19 downto 0)
		);
	end component;

	component muxb_263 is
		port (
			in_sel : in  std_logic;
			out_data : out std_logic;
			in_data0 : in  std_logic;
			in_data1 : in  std_logic
		);
	end component;

	component muxb_265 is
		port (
			in_sel : in  std_logic;
			out_data : out std_logic;
			in_data0 : in  std_logic;
			in_data1 : in  std_logic
		);
	end component;

	component add_277 is
		port (
			result : out std_logic_vector(19 downto 0);
			in_a : in  std_logic_vector(19 downto 0);
			in_b : in  std_logic_vector(19 downto 0)
		);
	end component;

	component add_295 is
		port (
			result : out std_logic_vector(26 downto 0);
			in_a : in  std_logic_vector(26 downto 0);
			in_b : in  std_logic_vector(26 downto 0)
		);
	end component;

	component add_296 is
		port (
			result : out std_logic_vector(19 downto 0);
			in_a : in  std_logic_vector(19 downto 0);
			in_b : in  std_logic_vector(19 downto 0)
		);
	end component;

	component sub_303 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component add_315 is
		port (
			result : out std_logic_vector(19 downto 0);
			in_a : in  std_logic_vector(19 downto 0);
			in_b : in  std_logic_vector(19 downto 0)
		);
	end component;

	component muxb_322 is
		port (
			in_sel : in  std_logic;
			out_data : out std_logic;
			in_data0 : in  std_logic;
			in_data1 : in  std_logic
		);
	end component;

	component add_323 is
		port (
			result : out std_logic_vector(15 downto 0);
			in_a : in  std_logic_vector(15 downto 0);
			in_b : in  std_logic_vector(15 downto 0)
		);
	end component;

	component cmp_324 is
		port (
			ne : out std_logic;
			in0 : in  std_logic_vector(15 downto 0);
			in1 : in  std_logic_vector(15 downto 0)
		);
	end component;

	component cmp_325 is
		port (
			eq : out std_logic;
			in0 : in  std_logic;
			in1 : in  std_logic
		);
	end component;

	component mul_328 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(14 downto 0)
		);
	end component;

	component mul_331 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(15 downto 0)
		);
	end component;

	component sub_337 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component add_338 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component mul_344 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(14 downto 0)
		);
	end component;

	component sub_345 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component add_350 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component mul_353 is
		port (
			result : out std_logic_vector(30 downto 0);
			in_a : in  std_logic_vector(30 downto 0);
			in_b : in  std_logic_vector(14 downto 0)
		);
	end component;

	component sub_354 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component mul_373 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(15 downto 0)
		);
	end component;

	component add_382 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component mul_383 is
		port (
			result : out std_logic_vector(30 downto 0);
			in_a : in  std_logic_vector(30 downto 0);
			in_b : in  std_logic_vector(14 downto 0)
		);
	end component;

	component add_390 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component sub_391 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component cmp_392 is
		port (
			ne : out std_logic;
			in0 : in  std_logic_vector(31 downto 0);
			in1 : in  std_logic_vector(31 downto 0)
		);
	end component;

	component add_393 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component cmp_396 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component cmp_402 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component cmp_411 is
		port (
			eq : out std_logic;
			in0 : in  std_logic;
			in1 : in  std_logic
		);
	end component;

	component cmp_413 is
		port (
			ne : out std_logic;
			in0 : in  std_logic_vector(31 downto 0);
			in1 : in  std_logic_vector(31 downto 0)
		);
	end component;

	component mul_416 is
		port (
			result : out std_logic_vector(30 downto 0);
			in_a : in  std_logic_vector(30 downto 0);
			in_b : in  std_logic_vector(14 downto 0)
		);
	end component;

	component add_419 is
		port (
			result : out std_logic_vector(26 downto 0);
			in_a : in  std_logic_vector(26 downto 0);
			in_b : in  std_logic_vector(26 downto 0)
		);
	end component;

	component add_430 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component sub_437 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component mul_442 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(15 downto 0)
		);
	end component;

	component mul_445 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(14 downto 0)
		);
	end component;

	component mul_447 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(14 downto 0)
		);
	end component;

	component mul_448 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(15 downto 0)
		);
	end component;

	component sub_449 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component mul_460 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(14 downto 0)
		);
	end component;

	component mul_469 is
		port (
			result : out std_logic_vector(30 downto 0);
			in_a : in  std_logic_vector(30 downto 0);
			in_b : in  std_logic_vector(14 downto 0)
		);
	end component;

	component add_474 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component mul_477 is
		port (
			result : out std_logic_vector(30 downto 0);
			in_a : in  std_logic_vector(30 downto 0);
			in_b : in  std_logic_vector(14 downto 0)
		);
	end component;

	component sub_478 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component add_483 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component sub_484 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component add_487 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component sub_488 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component sub_489 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component mul_492 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(14 downto 0)
		);
	end component;

	component mul_495 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(15 downto 0)
		);
	end component;

	component mul_499 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(15 downto 0)
		);
	end component;

	component mul_502 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(14 downto 0)
		);
	end component;

	component sub_503 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component add_508 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component mul_511 is
		port (
			result : out std_logic_vector(30 downto 0);
			in_a : in  std_logic_vector(30 downto 0);
			in_b : in  std_logic_vector(14 downto 0)
		);
	end component;

	component add_516 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component mul_520 is
		port (
			result : out std_logic_vector(30 downto 0);
			in_a : in  std_logic_vector(30 downto 0);
			in_b : in  std_logic_vector(14 downto 0)
		);
	end component;

	component mul_524 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(14 downto 0)
		);
	end component;

	component mul_527 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(15 downto 0)
		);
	end component;

	component mul_531 is
		port (
			result : out std_logic_vector(30 downto 0);
			in_a : in  std_logic_vector(30 downto 0);
			in_b : in  std_logic_vector(14 downto 0)
		);
	end component;

	component mul_534 is
		port (
			result : out std_logic_vector(30 downto 0);
			in_a : in  std_logic_vector(30 downto 0);
			in_b : in  std_logic_vector(14 downto 0)
		);
	end component;

	component add_537 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component mul_540 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(15 downto 0)
		);
	end component;

	component mul_543 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(14 downto 0)
		);
	end component;

	component sub_544 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component mul_547 is
		port (
			result : out std_logic_vector(30 downto 0);
			in_a : in  std_logic_vector(30 downto 0);
			in_b : in  std_logic_vector(14 downto 0)
		);
	end component;

	component add_552 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component sub_553 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component mul_556 is
		port (
			result : out std_logic_vector(30 downto 0);
			in_a : in  std_logic_vector(30 downto 0);
			in_b : in  std_logic_vector(14 downto 0)
		);
	end component;

	component mul_559 is
		port (
			result : out std_logic_vector(30 downto 0);
			in_a : in  std_logic_vector(30 downto 0);
			in_b : in  std_logic_vector(14 downto 0)
		);
	end component;

	component mul_561 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(15 downto 0)
		);
	end component;

	component sub_562 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component sub_563 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component add_564 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component mul_566 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(14 downto 0)
		);
	end component;

	component sub_567 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component add_570 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component mul_573 is
		port (
			result : out std_logic_vector(30 downto 0);
			in_a : in  std_logic_vector(30 downto 0);
			in_b : in  std_logic_vector(14 downto 0)
		);
	end component;

	component sub_574 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component mul_577 is
		port (
			result : out std_logic_vector(30 downto 0);
			in_a : in  std_logic_vector(30 downto 0);
			in_b : in  std_logic_vector(14 downto 0)
		);
	end component;

	component mul_579 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(15 downto 0)
		);
	end component;

	component sub_580 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component sub_585 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component sub_586 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component mul_589 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(15 downto 0)
		);
	end component;

	component mul_592 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(14 downto 0)
		);
	end component;

	component sub_593 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component mul_594 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(14 downto 0)
		);
	end component;

	component mul_595 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(15 downto 0)
		);
	end component;

	component sub_596 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component sub_599 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component add_600 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component add_601 is
		port (
			result : out std_logic_vector(26 downto 0);
			in_a : in  std_logic_vector(26 downto 0);
			in_b : in  std_logic_vector(26 downto 0)
		);
	end component;

	component add_602 is
		port (
			result : out std_logic_vector(19 downto 0);
			in_a : in  std_logic_vector(19 downto 0);
			in_b : in  std_logic_vector(19 downto 0)
		);
	end component;

	component mul_605 is
		port (
			result : out std_logic_vector(30 downto 0);
			in_a : in  std_logic_vector(30 downto 0);
			in_b : in  std_logic_vector(14 downto 0)
		);
	end component;

	-- Declaration of signals

	signal sig_clock : std_logic;
	signal sig_reset : std_logic;
	signal augh_test_9 : std_logic;
	signal augh_test_11 : std_logic;
	signal sig_start : std_logic;
	signal test_cp_0_16 : std_logic;
	signal test_cp_1_17 : std_logic;
	signal memextrct_loop_sig_21 : std_logic;
	signal psc_loop_sig_20 : std_logic;
	signal memextrct_loop_sig_22 : std_logic;
	signal sig_606 : std_logic_vector(30 downto 0);
	signal sig_607 : std_logic_vector(19 downto 0);
	signal sig_608 : std_logic_vector(26 downto 0);
	signal sig_609 : std_logic_vector(31 downto 0);
	signal sig_610 : std_logic_vector(31 downto 0);
	signal sig_611 : std_logic_vector(31 downto 0);
	signal sig_612 : std_logic_vector(31 downto 0);
	signal sig_613 : std_logic_vector(31 downto 0);
	signal sig_614 : std_logic_vector(31 downto 0);
	signal sig_615 : std_logic_vector(31 downto 0);
	signal sig_616 : std_logic_vector(31 downto 0);
	signal sig_617 : std_logic_vector(31 downto 0);
	signal sig_618 : std_logic_vector(31 downto 0);
	signal sig_619 : std_logic_vector(31 downto 0);
	signal sig_620 : std_logic_vector(31 downto 0);
	signal sig_621 : std_logic_vector(30 downto 0);
	signal sig_622 : std_logic_vector(31 downto 0);
	signal sig_623 : std_logic_vector(30 downto 0);
	signal sig_624 : std_logic_vector(31 downto 0);
	signal sig_625 : std_logic_vector(31 downto 0);
	signal sig_626 : std_logic_vector(31 downto 0);
	signal sig_627 : std_logic_vector(31 downto 0);
	signal sig_628 : std_logic_vector(31 downto 0);
	signal sig_629 : std_logic_vector(31 downto 0);
	signal sig_630 : std_logic_vector(31 downto 0);
	signal sig_631 : std_logic_vector(30 downto 0);
	signal sig_632 : std_logic_vector(30 downto 0);
	signal sig_633 : std_logic_vector(31 downto 0);
	signal sig_634 : std_logic_vector(31 downto 0);
	signal sig_635 : std_logic_vector(30 downto 0);
	signal sig_636 : std_logic_vector(31 downto 0);
	signal sig_637 : std_logic_vector(31 downto 0);
	signal sig_638 : std_logic_vector(31 downto 0);
	signal sig_639 : std_logic_vector(31 downto 0);
	signal sig_640 : std_logic_vector(30 downto 0);
	signal sig_641 : std_logic_vector(30 downto 0);
	signal sig_642 : std_logic_vector(31 downto 0);
	signal sig_643 : std_logic_vector(31 downto 0);
	signal sig_644 : std_logic_vector(30 downto 0);
	signal sig_645 : std_logic_vector(31 downto 0);
	signal sig_646 : std_logic_vector(30 downto 0);
	signal sig_647 : std_logic_vector(31 downto 0);
	signal sig_648 : std_logic_vector(31 downto 0);
	signal sig_649 : std_logic_vector(31 downto 0);
	signal sig_650 : std_logic_vector(31 downto 0);
	signal sig_651 : std_logic_vector(31 downto 0);
	signal sig_652 : std_logic_vector(31 downto 0);
	signal sig_653 : std_logic_vector(31 downto 0);
	signal sig_654 : std_logic_vector(31 downto 0);
	signal sig_655 : std_logic_vector(31 downto 0);
	signal sig_656 : std_logic_vector(31 downto 0);
	signal sig_657 : std_logic_vector(31 downto 0);
	signal sig_658 : std_logic_vector(31 downto 0);
	signal sig_659 : std_logic_vector(30 downto 0);
	signal sig_660 : std_logic_vector(31 downto 0);
	signal sig_661 : std_logic_vector(30 downto 0);
	signal sig_662 : std_logic_vector(31 downto 0);
	signal sig_663 : std_logic_vector(31 downto 0);
	signal sig_664 : std_logic_vector(31 downto 0);
	signal sig_665 : std_logic_vector(31 downto 0);
	signal sig_666 : std_logic_vector(31 downto 0);
	signal sig_667 : std_logic_vector(31 downto 0);
	signal sig_668 : std_logic_vector(31 downto 0);
	signal sig_669 : std_logic_vector(31 downto 0);
	signal sig_670 : std_logic_vector(26 downto 0);
	signal sig_671 : std_logic_vector(30 downto 0);
	signal sig_672 : std_logic;
	signal sig_673 : std_logic;
	signal sig_674 : std_logic;
	signal sig_675 : std_logic_vector(31 downto 0);
	signal sig_676 : std_logic_vector(31 downto 0);
	signal sig_677 : std_logic_vector(31 downto 0);
	signal sig_678 : std_logic_vector(30 downto 0);
	signal sig_679 : std_logic_vector(31 downto 0);
	signal sig_680 : std_logic_vector(31 downto 0);
	signal sig_681 : std_logic_vector(31 downto 0);
	signal sig_682 : std_logic_vector(30 downto 0);
	signal sig_683 : std_logic_vector(31 downto 0);
	signal sig_684 : std_logic_vector(31 downto 0);
	signal sig_685 : std_logic_vector(31 downto 0);
	signal sig_686 : std_logic_vector(31 downto 0);
	signal sig_687 : std_logic_vector(31 downto 0);
	signal sig_688 : std_logic_vector(31 downto 0);
	signal sig_689 : std_logic_vector(31 downto 0);
	signal sig_690 : std_logic;
	signal sig_691 : std_logic_vector(15 downto 0);
	signal sig_692 : std_logic;
	signal sig_693 : std_logic_vector(19 downto 0);
	signal sig_694 : std_logic_vector(31 downto 0);
	signal sig_695 : std_logic_vector(19 downto 0);
	signal sig_696 : std_logic_vector(26 downto 0);
	signal sig_697 : std_logic_vector(19 downto 0);
	signal sig_698 : std_logic;
	signal sig_699 : std_logic;
	signal sig_700 : std_logic_vector(19 downto 0);
	signal sig_701 : std_logic_vector(31 downto 0);
	signal sig_702 : std_logic;
	signal sig_703 : std_logic;
	signal sig_704 : std_logic_vector(31 downto 0);
	signal sig_705 : std_logic;
	signal sig_706 : std_logic_vector(31 downto 0);
	signal sig_707 : std_logic_vector(31 downto 0);
	signal sig_708 : std_logic_vector(31 downto 0);
	signal sig_709 : std_logic_vector(31 downto 0);
	signal sig_710 : std_logic_vector(31 downto 0);
	signal sig_711 : std_logic_vector(31 downto 0);
	signal sig_712 : std_logic_vector(30 downto 0);
	signal sig_713 : std_logic_vector(31 downto 0);
	signal sig_714 : std_logic_vector(19 downto 0);
	signal sig_715 : std_logic_vector(31 downto 0);
	signal sig_716 : std_logic_vector(31 downto 0);
	signal sig_717 : std_logic_vector(19 downto 0);
	signal sig_718 : std_logic_vector(26 downto 0);
	signal sig_719 : std_logic_vector(26 downto 0);
	signal sig_720 : std_logic_vector(26 downto 0);
	signal sig_721 : std_logic;
	signal sig_722 : std_logic;
	signal sig_723 : std_logic;
	signal sig_724 : std_logic;
	signal sig_725 : std_logic;
	signal sig_726 : std_logic;
	signal sig_727 : std_logic;
	signal sig_728 : std_logic;
	signal sig_729 : std_logic;
	signal sig_730 : std_logic;
	signal sig_731 : std_logic;
	signal sig_732 : std_logic;
	signal sig_733 : std_logic;
	signal sig_734 : std_logic;
	signal sig_735 : std_logic;
	signal sig_736 : std_logic;
	signal sig_737 : std_logic;
	signal sig_738 : std_logic;
	signal sig_739 : std_logic;
	signal sig_740 : std_logic;
	signal sig_741 : std_logic;
	signal sig_742 : std_logic;
	signal sig_743 : std_logic;
	signal sig_744 : std_logic;
	signal sig_745 : std_logic;
	signal sig_746 : std_logic;
	signal sig_747 : std_logic;
	signal sig_748 : std_logic;
	signal sig_749 : std_logic;
	signal sig_750 : std_logic;
	signal sig_751 : std_logic;
	signal sig_752 : std_logic;
	signal sig_753 : std_logic;
	signal sig_754 : std_logic;
	signal sig_755 : std_logic;
	signal sig_756 : std_logic;
	signal sig_757 : std_logic;
	signal sig_758 : std_logic;
	signal sig_759 : std_logic;
	signal sig_760 : std_logic;
	signal sig_761 : std_logic;
	signal sig_762 : std_logic;
	signal sig_763 : std_logic;
	signal sig_764 : std_logic;
	signal sig_765 : std_logic;
	signal sig_766 : std_logic;
	signal sig_767 : std_logic;
	signal sig_768 : std_logic;
	signal sig_769 : std_logic;
	signal sig_770 : std_logic;
	signal sig_771 : std_logic;
	signal sig_772 : std_logic;
	signal sig_773 : std_logic;
	signal sig_774 : std_logic;
	signal sig_775 : std_logic;
	signal sig_776 : std_logic;
	signal sig_777 : std_logic;
	signal sig_778 : std_logic;
	signal sig_779 : std_logic;
	signal sig_780 : std_logic;
	signal sig_781 : std_logic;
	signal sig_782 : std_logic;
	signal sig_783 : std_logic;
	signal sig_784 : std_logic;
	signal sig_785 : std_logic;
	signal sig_786 : std_logic;
	signal sig_787 : std_logic;
	signal sig_788 : std_logic;
	signal sig_789 : std_logic;
	signal sig_790 : std_logic;
	signal sig_791 : std_logic;
	signal sig_792 : std_logic;
	signal sig_793 : std_logic;
	signal sig_794 : std_logic;
	signal sig_795 : std_logic;
	signal sig_796 : std_logic;
	signal sig_797 : std_logic;
	signal sig_798 : std_logic;
	signal sig_799 : std_logic;
	signal sig_800 : std_logic;
	signal sig_801 : std_logic;
	signal sig_802 : std_logic;
	signal sig_803 : std_logic;
	signal sig_804 : std_logic;
	signal sig_805 : std_logic;
	signal sig_806 : std_logic;
	signal sig_807 : std_logic;
	signal sig_808 : std_logic;
	signal sig_809 : std_logic;
	signal sig_810 : std_logic;
	signal sig_811 : std_logic;
	signal sig_812 : std_logic;
	signal sig_813 : std_logic;
	signal sig_814 : std_logic;
	signal sig_815 : std_logic;
	signal sig_816 : std_logic;
	signal sig_817 : std_logic;
	signal sig_818 : std_logic;
	signal sig_819 : std_logic;
	signal sig_820 : std_logic;
	signal sig_821 : std_logic;
	signal sig_822 : std_logic;
	signal sig_823 : std_logic;
	signal sig_824 : std_logic;
	signal sig_825 : std_logic;
	signal sig_826 : std_logic;
	signal sig_827 : std_logic;
	signal sig_828 : std_logic;
	signal sig_829 : std_logic;
	signal sig_830 : std_logic;
	signal sig_831 : std_logic;
	signal sig_832 : std_logic;
	signal sig_833 : std_logic;
	signal sig_834 : std_logic;
	signal sig_835 : std_logic;
	signal sig_836 : std_logic;
	signal sig_837 : std_logic;
	signal sig_838 : std_logic;
	signal sig_839 : std_logic;
	signal sig_840 : std_logic;
	signal sig_841 : std_logic;
	signal sig_842 : std_logic;
	signal sig_843 : std_logic;
	signal sig_844 : std_logic;
	signal sig_845 : std_logic;
	signal sig_846 : std_logic;
	signal sig_847 : std_logic;
	signal sig_848 : std_logic;
	signal sig_849 : std_logic;
	signal sig_850 : std_logic;
	signal sig_851 : std_logic;
	signal sig_852 : std_logic;
	signal sig_853 : std_logic;
	signal sig_854 : std_logic;
	signal sig_855 : std_logic;
	signal sig_856 : std_logic;
	signal sig_857 : std_logic;
	signal sig_858 : std_logic;
	signal sig_859 : std_logic;
	signal sig_860 : std_logic;
	signal sig_861 : std_logic;
	signal sig_862 : std_logic;
	signal sig_863 : std_logic;
	signal sig_864 : std_logic;
	signal sig_865 : std_logic;
	signal sig_866 : std_logic;
	signal sig_867 : std_logic;
	signal sig_868 : std_logic;
	signal sig_869 : std_logic;
	signal sig_870 : std_logic;
	signal sig_871 : std_logic;
	signal sig_872 : std_logic;
	signal sig_873 : std_logic;
	signal sig_874 : std_logic;
	signal sig_875 : std_logic;
	signal sig_876 : std_logic;
	signal sig_877 : std_logic;
	signal sig_878 : std_logic;
	signal sig_879 : std_logic;
	signal sig_880 : std_logic;
	signal sig_881 : std_logic;
	signal sig_882 : std_logic;
	signal sig_883 : std_logic;
	signal sig_884 : std_logic;
	signal sig_885 : std_logic;
	signal sig_886 : std_logic;
	signal sig_887 : std_logic;
	signal sig_888 : std_logic;
	signal sig_889 : std_logic;
	signal sig_890 : std_logic;
	signal sig_891 : std_logic;
	signal sig_892 : std_logic;
	signal sig_893 : std_logic;
	signal sig_894 : std_logic;
	signal sig_895 : std_logic;
	signal sig_896 : std_logic;
	signal sig_897 : std_logic;
	signal sig_898 : std_logic;
	signal sig_899 : std_logic;
	signal sig_900 : std_logic;
	signal sig_901 : std_logic;
	signal sig_902 : std_logic;
	signal sig_903 : std_logic;
	signal sig_904 : std_logic;
	signal sig_905 : std_logic;
	signal sig_906 : std_logic;
	signal sig_907 : std_logic;
	signal sig_908 : std_logic;
	signal sig_909 : std_logic;
	signal sig_910 : std_logic;
	signal sig_911 : std_logic;
	signal sig_912 : std_logic;
	signal sig_913 : std_logic;
	signal sig_914 : std_logic;
	signal sig_915 : std_logic;
	signal sig_916 : std_logic;
	signal sig_917 : std_logic;
	signal sig_918 : std_logic;
	signal sig_919 : std_logic;
	signal sig_920 : std_logic;
	signal sig_921 : std_logic;
	signal sig_922 : std_logic;
	signal sig_923 : std_logic;
	signal sig_924 : std_logic;
	signal sig_925 : std_logic;
	signal sig_926 : std_logic;
	signal sig_927 : std_logic;
	signal sig_928 : std_logic;
	signal sig_929 : std_logic;
	signal sig_930 : std_logic;
	signal sig_931 : std_logic;
	signal sig_932 : std_logic;
	signal sig_933 : std_logic;
	signal sig_934 : std_logic;
	signal sig_935 : std_logic;
	signal sig_936 : std_logic;
	signal sig_937 : std_logic;
	signal sig_938 : std_logic;
	signal sig_939 : std_logic;
	signal sig_940 : std_logic;
	signal sig_941 : std_logic;
	signal sig_942 : std_logic;
	signal sig_943 : std_logic;
	signal sig_944 : std_logic;
	signal sig_945 : std_logic;
	signal sig_946 : std_logic;
	signal sig_947 : std_logic;
	signal sig_948 : std_logic;
	signal sig_949 : std_logic;
	signal sig_950 : std_logic;
	signal sig_951 : std_logic;
	signal sig_952 : std_logic;
	signal sig_953 : std_logic;
	signal sig_954 : std_logic;
	signal sig_955 : std_logic;
	signal sig_956 : std_logic;
	signal sig_957 : std_logic;
	signal sig_958 : std_logic;
	signal sig_959 : std_logic;
	signal sig_960 : std_logic;
	signal sig_961 : std_logic;
	signal sig_962 : std_logic;
	signal sig_963 : std_logic;
	signal sig_964 : std_logic;
	signal sig_965 : std_logic;
	signal sig_966 : std_logic;
	signal sig_967 : std_logic;
	signal sig_968 : std_logic;
	signal sig_969 : std_logic;
	signal sig_970 : std_logic;
	signal sig_971 : std_logic;
	signal sig_972 : std_logic;
	signal sig_973 : std_logic;
	signal sig_974 : std_logic;
	signal sig_975 : std_logic;
	signal sig_976 : std_logic;
	signal sig_977 : std_logic;
	signal sig_978 : std_logic;
	signal sig_979 : std_logic;
	signal sig_980 : std_logic;
	signal sig_981 : std_logic;
	signal sig_982 : std_logic;
	signal sig_983 : std_logic;
	signal sig_984 : std_logic;
	signal sig_985 : std_logic;
	signal sig_986 : std_logic;
	signal sig_987 : std_logic;
	signal sig_988 : std_logic;
	signal sig_989 : std_logic;
	signal sig_990 : std_logic;
	signal sig_991 : std_logic;
	signal sig_992 : std_logic;
	signal sig_993 : std_logic;
	signal sig_994 : std_logic;
	signal sig_995 : std_logic;
	signal sig_996 : std_logic;
	signal sig_997 : std_logic;
	signal sig_998 : std_logic;
	signal sig_999 : std_logic;
	signal sig_1000 : std_logic;
	signal sig_1001 : std_logic;
	signal sig_1002 : std_logic;
	signal sig_1003 : std_logic;
	signal sig_1004 : std_logic;
	signal sig_1005 : std_logic;
	signal sig_1006 : std_logic;
	signal sig_1007 : std_logic;
	signal sig_1008 : std_logic;
	signal sig_1009 : std_logic;
	signal sig_1010 : std_logic;
	signal sig_1011 : std_logic;
	signal sig_1012 : std_logic;
	signal sig_1013 : std_logic;
	signal sig_1014 : std_logic;
	signal sig_1015 : std_logic;
	signal sig_1016 : std_logic;
	signal sig_1017 : std_logic;
	signal sig_1018 : std_logic;
	signal sig_1019 : std_logic;
	signal sig_1020 : std_logic;
	signal sig_1021 : std_logic;
	signal sig_1022 : std_logic;
	signal sig_1023 : std_logic;
	signal sig_1024 : std_logic;
	signal sig_1025 : std_logic;
	signal sig_1026 : std_logic;
	signal sig_1027 : std_logic;
	signal sig_1028 : std_logic;
	signal sig_1029 : std_logic;
	signal sig_1030 : std_logic;
	signal sig_1031 : std_logic;
	signal sig_1032 : std_logic;
	signal sig_1033 : std_logic;
	signal sig_1034 : std_logic;
	signal sig_1035 : std_logic;
	signal sig_1036 : std_logic;
	signal sig_1037 : std_logic;
	signal sig_1038 : std_logic;
	signal sig_1039 : std_logic;
	signal sig_1040 : std_logic;
	signal sig_1041 : std_logic;
	signal sig_1042 : std_logic;
	signal sig_1043 : std_logic;
	signal sig_1044 : std_logic;
	signal sig_1045 : std_logic;
	signal sig_1046 : std_logic;
	signal sig_1047 : std_logic;
	signal sig_1048 : std_logic;
	signal sig_1049 : std_logic;
	signal sig_1050 : std_logic;
	signal sig_1051 : std_logic;
	signal sig_1052 : std_logic;
	signal sig_1053 : std_logic;
	signal sig_1054 : std_logic;
	signal sig_1055 : std_logic;
	signal sig_1056 : std_logic;
	signal sig_1057 : std_logic;
	signal sig_1058 : std_logic;
	signal sig_1059 : std_logic_vector(31 downto 0);
	signal sig_1060 : std_logic_vector(31 downto 0);
	signal sig_1061 : std_logic_vector(31 downto 0);
	signal sig_1062 : std_logic_vector(31 downto 0);
	signal sig_1063 : std_logic_vector(31 downto 0);
	signal sig_1064 : std_logic;
	signal sig_1065 : std_logic;
	signal sig_1066 : std_logic;
	signal sig_1067 : std_logic;
	signal sig_1068 : std_logic;
	signal sig_1069 : std_logic;
	signal sig_1070 : std_logic;
	signal sig_1071 : std_logic_vector(31 downto 0);
	signal sig_1072 : std_logic_vector(31 downto 0);
	signal sig_1073 : std_logic_vector(31 downto 0);
	signal sig_1074 : std_logic_vector(31 downto 0);
	signal sig_1075 : std_logic_vector(31 downto 0);
	signal sig_1076 : std_logic_vector(30 downto 0);
	signal sig_1077 : std_logic_vector(31 downto 0);
	signal sig_1078 : std_logic_vector(31 downto 0);
	signal sig_1079 : std_logic_vector(31 downto 0);
	signal sig_1080 : std_logic_vector(19 downto 0);
	signal sig_1081 : std_logic_vector(19 downto 0);
	signal sig_1082 : std_logic_vector(19 downto 0);
	signal sig_1083 : std_logic_vector(31 downto 0);
	signal sig_1084 : std_logic_vector(31 downto 0);
	signal sig_1085 : std_logic_vector(31 downto 0);
	signal sig_1086 : std_logic_vector(31 downto 0);
	signal sig_1087 : std_logic_vector(31 downto 0);
	signal sig_1088 : std_logic_vector(26 downto 0);
	signal sig_1089 : std_logic_vector(26 downto 0);
	signal sig_1090 : std_logic_vector(31 downto 0);
	signal sig_1091 : std_logic_vector(29 downto 0);
	signal sig_1092 : std_logic_vector(31 downto 0);
	signal sig_1093 : std_logic_vector(31 downto 0);
	signal sig_1094 : std_logic_vector(31 downto 0);
	signal sig_1095 : std_logic_vector(31 downto 0);
	signal sig_1096 : std_logic_vector(7 downto 0);
	signal sig_1097 : std_logic_vector(7 downto 0);
	signal sig_1098 : std_logic_vector(7 downto 0);
	signal sig_1099 : std_logic_vector(7 downto 0);
	signal sig_1100 : std_logic_vector(31 downto 0);
	signal sig_1101 : std_logic_vector(31 downto 0);
	signal sig_1102 : std_logic_vector(31 downto 0);
	signal sig_1103 : std_logic_vector(31 downto 0);
	signal sig_1104 : std_logic_vector(26 downto 0);
	signal sig_1105 : std_logic_vector(31 downto 0);
	signal sig_1106 : std_logic;
	signal sig_1107 : std_logic_vector(26 downto 0);
	signal sig_1108 : std_logic_vector(26 downto 0);
	signal sig_1109 : std_logic_vector(31 downto 0);
	signal sig_1110 : std_logic_vector(31 downto 0);
	signal sig_1111 : std_logic_vector(31 downto 0);
	signal sig_1112 : std_logic_vector(30 downto 0);
	signal sig_1113 : std_logic_vector(31 downto 0);
	signal sig_1114 : std_logic_vector(31 downto 0);
	signal sig_1115 : std_logic_vector(31 downto 0);
	signal sig_1116 : std_logic_vector(26 downto 0);
	signal sig_1117 : std_logic_vector(26 downto 0);
	signal sig_1118 : std_logic_vector(26 downto 0);
	signal sig_1119 : std_logic_vector(31 downto 0);
	signal sig_1120 : std_logic_vector(31 downto 0);
	signal sig_1121 : std_logic_vector(31 downto 0);
	signal sig_1122 : std_logic_vector(29 downto 0);
	signal sig_1123 : std_logic_vector(31 downto 0);
	signal sig_1124 : std_logic_vector(31 downto 0);
	signal sig_1125 : std_logic_vector(19 downto 0);
	signal sig_1126 : std_logic_vector(19 downto 0);
	signal sig_1127 : std_logic_vector(19 downto 0);
	signal sig_1128 : std_logic_vector(15 downto 0);
	signal sig_1129 : std_logic_vector(31 downto 0);
	signal sig_1130 : std_logic;
	signal sig_1131 : std_logic_vector(31 downto 0);
	signal sig_1132 : std_logic_vector(30 downto 0);
	signal sig_1133 : std_logic_vector(31 downto 0);
	signal sig_1134 : std_logic_vector(31 downto 0);
	signal sig_1135 : std_logic_vector(31 downto 0);
	signal sig_1136 : std_logic_vector(31 downto 0);
	signal sig_1137 : std_logic_vector(31 downto 0);
	signal sig_1138 : std_logic_vector(31 downto 0);
	signal sig_1139 : std_logic_vector(31 downto 0);
	signal sig_1140 : std_logic_vector(31 downto 0);
	signal sig_1141 : std_logic_vector(31 downto 0);
	signal sig_1142 : std_logic_vector(31 downto 0);
	signal sig_1143 : std_logic_vector(30 downto 0);
	signal sig_1144 : std_logic_vector(31 downto 0);
	signal sig_1145 : std_logic_vector(31 downto 0);
	signal sig_1146 : std_logic_vector(29 downto 0);
	signal sig_1147 : std_logic_vector(30 downto 0);
	signal sig_1148 : std_logic_vector(30 downto 0);
	signal sig_1149 : std_logic_vector(31 downto 0);
	signal sig_1150 : std_logic_vector(31 downto 0);
	signal sig_1151 : std_logic_vector(19 downto 0);
	signal sig_1152 : std_logic_vector(19 downto 0);
	signal sig_1153 : std_logic_vector(7 downto 0);
	signal sig_1154 : std_logic_vector(7 downto 0);
	signal sig_1155 : std_logic_vector(26 downto 0);
	signal sig_1156 : std_logic_vector(31 downto 0);
	signal sig_1157 : std_logic;
	signal sig_1158 : std_logic_vector(7 downto 0);
	signal sig_1159 : std_logic_vector(7 downto 0);
	signal sig_1160 : std_logic_vector(19 downto 0);
	signal sig_1161 : std_logic_vector(31 downto 0);
	signal sig_1162 : std_logic_vector(31 downto 0);
	signal sig_1163 : std_logic_vector(19 downto 0);
	signal sig_1164 : std_logic_vector(31 downto 0);
	signal sig_1165 : std_logic_vector(19 downto 0);
	signal sig_1166 : std_logic_vector(19 downto 0);
	signal sig_1167 : std_logic_vector(19 downto 0);
	signal sig_1168 : std_logic_vector(19 downto 0);
	signal sig_1169 : std_logic_vector(19 downto 0);
	signal sig_1170 : std_logic_vector(31 downto 0);
	signal sig_1171 : std_logic_vector(19 downto 0);
	signal sig_1172 : std_logic_vector(19 downto 0);
	signal sig_1173 : std_logic_vector(19 downto 0);
	signal sig_1174 : std_logic_vector(31 downto 0);
	signal sig_1175 : std_logic_vector(31 downto 0);
	signal sig_1176 : std_logic_vector(31 downto 0);
	signal sig_1177 : std_logic_vector(31 downto 0);
	signal sig_1178 : std_logic_vector(31 downto 0);
	signal sig_1179 : std_logic_vector(19 downto 0);
	signal sig_1180 : std_logic_vector(19 downto 0);
	signal sig_1181 : std_logic_vector(19 downto 0);
	signal sig_1182 : std_logic_vector(19 downto 0);
	signal sig_1183 : std_logic_vector(19 downto 0);

	-- Other inlined components

	signal mux_66 : std_logic_vector(2 downto 0);
	signal mux_30 : std_logic;
	signal mux_32 : std_logic;
	signal mux_33 : std_logic;
	signal mux_34 : std_logic;
	signal augh_main_k : std_logic_vector(31 downto 0) := (others => '0');
	signal read32_ret0_10 : std_logic_vector(31 downto 0) := (others => '0');
	signal mux_58 : std_logic_vector(2 downto 0);
	signal mux_59 : std_logic_vector(2 downto 0);
	signal mux_60 : std_logic;
	signal mux_61 : std_logic_vector(7 downto 0);
	signal mux_62 : std_logic_vector(2 downto 0);
	signal mux_63 : std_logic_vector(2 downto 0);
	signal mux_64 : std_logic;
	signal mux_65 : std_logic_vector(7 downto 0);
	signal mux_35 : std_logic;
	signal mux_36 : std_logic;
	signal mux_37 : std_logic_vector(15 downto 0);
	signal mux_38 : std_logic;
	signal mux_39 : std_logic_vector(31 downto 0);
	signal idct_2d_r : std_logic_vector(31 downto 0) := (others => '0');
	signal mux_45 : std_logic_vector(4 downto 0);
	signal mux_46 : std_logic_vector(4 downto 0);
	signal mux_47 : std_logic_vector(4 downto 0);
	signal mux_48 : std_logic_vector(4 downto 0);
	signal mux_49 : std_logic_vector(4 downto 0);
	signal mux_40 : std_logic_vector(4 downto 0);
	signal mux_41 : std_logic_vector(4 downto 0);
	signal mux_42 : std_logic;
	signal mux_43 : std_logic_vector(4 downto 0);
	signal mux_44 : std_logic_vector(4 downto 0);
	signal write8_u8 : std_logic_vector(7 downto 0) := (others => '0');
	signal mux_50 : std_logic_vector(31 downto 0);
	signal mux_51 : std_logic_vector(4 downto 0);
	signal mux_52 : std_logic;
	signal mux_53 : std_logic_vector(7 downto 0);
	signal mux_54 : std_logic_vector(2 downto 0);
	signal mux_55 : std_logic_vector(2 downto 0);
	signal mux_56 : std_logic;
	signal mux_57 : std_logic_vector(7 downto 0);
	signal idct_z3_reg4 : std_logic_vector(31 downto 0) := (others => '0');
	signal idct_z3_reg5 : std_logic_vector(31 downto 0) := (others => '0');
	signal idct_z3_reg6 : std_logic_vector(31 downto 0) := (others => '0');
	signal idct_z3_reg7 : std_logic_vector(31 downto 0) := (others => '0');
	signal idct_z1_reg0 : std_logic_vector(31 downto 0) := (others => '0');
	signal idct_z1_reg1 : std_logic_vector(31 downto 0) := (others => '0');
	signal idct_z1_reg2 : std_logic_vector(31 downto 0) := (others => '0');
	signal idct_z1_reg3 : std_logic_vector(31 downto 0) := (others => '0');
	signal idct_z1_reg4 : std_logic_vector(31 downto 0) := (others => '0');
	signal mux_88 : std_logic;
	signal mux_67 : std_logic_vector(2 downto 0);
	signal mux_68 : std_logic;
	signal mux_69 : std_logic_vector(31 downto 0);
	signal mux_71 : std_logic_vector(31 downto 0);
	signal mux_73 : std_logic_vector(31 downto 0);
	signal mux_75 : std_logic_vector(31 downto 0);
	signal mux_77 : std_logic_vector(31 downto 0);
	signal mux_79 : std_logic_vector(31 downto 0);
	signal mux_81 : std_logic_vector(31 downto 0);
	signal mux_83 : std_logic_vector(31 downto 0);
	signal mux_85 : std_logic_vector(7 downto 0);
	signal mux_86 : std_logic_vector(2 downto 0);
	signal mux_87 : std_logic_vector(2 downto 0);
	signal mux_28 : std_logic;
	signal idct_z1_reg5 : std_logic_vector(31 downto 0) := (others => '0');
	signal idct_z1_reg6 : std_logic_vector(31 downto 0) := (others => '0');
	signal idct_z1_reg7 : std_logic_vector(31 downto 0) := (others => '0');
	signal idct_z2_reg0 : std_logic_vector(31 downto 0) := (others => '0');
	signal idct_z2_reg1 : std_logic_vector(31 downto 0) := (others => '0');
	signal idct_z2_reg2 : std_logic_vector(31 downto 0) := (others => '0');
	signal idct_z2_reg3 : std_logic_vector(31 downto 0) := (others => '0');
	signal idct_z2_reg4 : std_logic_vector(31 downto 0) := (others => '0');
	signal idct_z2_reg5 : std_logic_vector(31 downto 0) := (others => '0');
	signal idct_z2_reg6 : std_logic_vector(31 downto 0) := (others => '0');
	signal idct_z2_reg7 : std_logic_vector(31 downto 0) := (others => '0');
	signal mux_109 : std_logic_vector(31 downto 0);
	signal mux_154 : std_logic;
	signal mux_156 : std_logic_vector(7 downto 0);
	signal idct_2d_yc_reg0 : std_logic_vector(31 downto 0) := (others => '0');
	signal idct_2d_yc_reg1 : std_logic_vector(31 downto 0) := (others => '0');
	signal idct_2d_yc_reg2 : std_logic_vector(31 downto 0) := (others => '0');
	signal idct_2d_yc_reg3 : std_logic_vector(31 downto 0) := (others => '0');
	signal idct_2d_yc_reg4 : std_logic_vector(31 downto 0) := (others => '0');
	signal idct_2d_yc_reg5 : std_logic_vector(31 downto 0) := (others => '0');
	signal idct_2d_yc_reg6 : std_logic_vector(31 downto 0) := (others => '0');
	signal idct_2d_yc_reg7 : std_logic_vector(31 downto 0) := (others => '0');
	signal mux_89 : std_logic_vector(7 downto 0);
	signal mux_90 : std_logic_vector(2 downto 0);
	signal mux_134 : std_logic;
	signal mux_91 : std_logic_vector(2 downto 0);
	signal mux_92 : std_logic;
	signal mux_158 : std_logic_vector(7 downto 0);
	signal mux_111 : std_logic_vector(31 downto 0);
	signal mux_113 : std_logic_vector(31 downto 0);
	signal mux_115 : std_logic_vector(31 downto 0);
	signal mux_117 : std_logic_vector(31 downto 0);
	signal mux_119 : std_logic_vector(31 downto 0);
	signal mux_121 : std_logic_vector(31 downto 0);
	signal mux_123 : std_logic_vector(31 downto 0);
	signal or_224 : std_logic_vector(31 downto 0);
	signal and_225 : std_logic_vector(31 downto 0);
	signal or_231 : std_logic_vector(31 downto 0);
	signal and_232 : std_logic_vector(31 downto 0);
	signal or_250 : std_logic_vector(31 downto 0);
	signal and_251 : std_logic_vector(31 downto 0);
	signal or_260 : std_logic_vector(31 downto 0);
	signal and_261 : std_logic_vector(31 downto 0);
	signal or_282 : std_logic_vector(31 downto 0);
	signal and_283 : std_logic_vector(31 downto 0);
	signal or_285 : std_logic_vector(31 downto 0);
	signal and_286 : std_logic_vector(31 downto 0);
	signal or_289 : std_logic_vector(31 downto 0);
	signal and_290 : std_logic_vector(31 downto 0);
	signal or_291 : std_logic_vector(31 downto 0);
	signal and_292 : std_logic_vector(31 downto 0);
	signal or_297 : std_logic_vector(31 downto 0);
	signal and_298 : std_logic_vector(31 downto 0);
	signal or_299 : std_logic_vector(31 downto 0);
	signal and_300 : std_logic_vector(31 downto 0);
	signal or_320 : std_logic_vector(31 downto 0);
	signal and_321 : std_logic_vector(31 downto 0);
	signal or_326 : std_logic_vector(31 downto 0);
	signal and_327 : std_logic_vector(31 downto 0);
	signal or_333 : std_logic_vector(31 downto 0);
	signal and_334 : std_logic_vector(31 downto 0);
	signal or_363 : std_logic_vector(31 downto 0);
	signal and_364 : std_logic_vector(31 downto 0);
	signal and_403 : std_logic_vector(7 downto 0);
	signal and_405 : std_logic_vector(7 downto 0);
	signal and_407 : std_logic_vector(7 downto 0);
	signal and_409 : std_logic_vector(7 downto 0);
	signal and_415 : std_logic_vector(30 downto 0);
	signal or_464 : std_logic_vector(31 downto 0);
	signal and_465 : std_logic_vector(31 downto 0);
	signal or_470 : std_logic_vector(31 downto 0);
	signal and_471 : std_logic_vector(31 downto 0);
	signal or_472 : std_logic_vector(31 downto 0);
	signal and_473 : std_logic_vector(31 downto 0);
	signal or_500 : std_logic_vector(31 downto 0);
	signal and_501 : std_logic_vector(31 downto 0);
	signal or_504 : std_logic_vector(31 downto 0);
	signal and_505 : std_logic_vector(31 downto 0);
	signal or_506 : std_logic_vector(31 downto 0);
	signal and_507 : std_logic_vector(31 downto 0);
	signal or_514 : std_logic_vector(31 downto 0);
	signal and_515 : std_logic_vector(31 downto 0);
	signal or_522 : std_logic_vector(31 downto 0);
	signal and_523 : std_logic_vector(31 downto 0);
	signal psc_loop_reg_13 : std_logic_vector(15 downto 0) := (others => '0');
	signal cp_id_reg_14 : std_logic := '0';
	signal cp_id_reg_stable_15 : std_logic := '0';
	signal psc_stuff_reg_18 : std_logic_vector(23 downto 0) := (others => '0');
	signal psc_stuff_reg_19 : std_logic_vector(62 downto 0) := "000000000000000000000000000000000000000000000000000000000000000";
	signal mux_129 : std_logic_vector(31 downto 0);
	signal mux_133 : std_logic_vector(7 downto 0);
	signal mux_135 : std_logic_vector(31 downto 0);
	signal mux_137 : std_logic_vector(7 downto 0);
	signal mux_138 : std_logic_vector(2 downto 0);
	signal mux_139 : std_logic_vector(2 downto 0);
	signal mux_140 : std_logic;
	signal mux_141 : std_logic_vector(7 downto 0);
	signal mux_142 : std_logic_vector(2 downto 0);
	signal mux_143 : std_logic_vector(2 downto 0);
	signal mux_144 : std_logic;
	signal mux_147 : std_logic;
	signal mux_149 : std_logic_vector(31 downto 0);
	signal mux_150 : std_logic;
	signal mux_151 : std_logic;
	signal mux_152 : std_logic_vector(63 downto 0);
	signal mux_155 : std_logic;
	signal or_221 : std_logic_vector(31 downto 0);
	signal and_222 : std_logic_vector(31 downto 0);
	signal or_233 : std_logic_vector(31 downto 0);
	signal and_234 : std_logic_vector(31 downto 0);
	signal or_237 : std_logic_vector(31 downto 0);
	signal and_238 : std_logic_vector(31 downto 0);
	signal or_252 : std_logic_vector(31 downto 0);
	signal and_253 : std_logic_vector(31 downto 0);
	signal or_256 : std_logic_vector(31 downto 0);
	signal and_257 : std_logic_vector(31 downto 0);
	signal or_268 : std_logic_vector(31 downto 0);
	signal and_269 : std_logic_vector(31 downto 0);
	signal or_270 : std_logic_vector(31 downto 0);
	signal and_271 : std_logic_vector(31 downto 0);
	signal or_274 : std_logic_vector(31 downto 0);
	signal and_275 : std_logic_vector(31 downto 0);
	signal or_278 : std_logic_vector(31 downto 0);
	signal and_279 : std_logic_vector(31 downto 0);
	signal or_310 : std_logic_vector(31 downto 0);
	signal and_311 : std_logic_vector(31 downto 0);
	signal or_316 : std_logic_vector(31 downto 0);
	signal and_317 : std_logic_vector(31 downto 0);
	signal or_358 : std_logic_vector(31 downto 0);
	signal and_359 : std_logic_vector(31 downto 0);
	signal or_366 : std_logic_vector(31 downto 0);
	signal and_367 : std_logic_vector(31 downto 0);
	signal or_374 : std_logic_vector(31 downto 0);
	signal and_375 : std_logic_vector(31 downto 0);
	signal or_417 : std_logic_vector(31 downto 0);
	signal and_418 : std_logic_vector(31 downto 0);
	signal or_421 : std_logic_vector(31 downto 0);
	signal and_422 : std_logic_vector(31 downto 0);
	signal or_435 : std_logic_vector(31 downto 0);
	signal and_436 : std_logic_vector(31 downto 0);
	signal or_452 : std_logic_vector(31 downto 0);
	signal and_453 : std_logic_vector(31 downto 0);
	signal and_494 : std_logic_vector(31 downto 0);
	signal and_498 : std_logic_vector(31 downto 0);
	signal or_509 : std_logic_vector(30 downto 0);
	signal and_510 : std_logic_vector(30 downto 0);
	signal or_550 : std_logic_vector(31 downto 0);
	signal and_551 : std_logic_vector(31 downto 0);
	signal or_581 : std_logic_vector(31 downto 0);
	signal and_582 : std_logic_vector(31 downto 0);
	signal or_583 : std_logic_vector(31 downto 0);
	signal and_584 : std_logic_vector(31 downto 0);
	signal or_587 : std_logic_vector(31 downto 0);
	signal and_588 : std_logic_vector(31 downto 0);
	signal and_161 : std_logic;
	signal or_228 : std_logic_vector(31 downto 0);
	signal and_229 : std_logic_vector(31 downto 0);
	signal or_239 : std_logic_vector(31 downto 0);
	signal and_240 : std_logic_vector(31 downto 0);
	signal or_241 : std_logic_vector(31 downto 0);
	signal and_242 : std_logic_vector(31 downto 0);
	signal or_244 : std_logic_vector(31 downto 0);
	signal and_245 : std_logic_vector(31 downto 0);
	signal or_246 : std_logic_vector(31 downto 0);
	signal and_247 : std_logic_vector(31 downto 0);
	signal or_248 : std_logic_vector(31 downto 0);
	signal and_249 : std_logic_vector(31 downto 0);
	signal or_258 : std_logic_vector(31 downto 0);
	signal and_259 : std_logic_vector(31 downto 0);
	signal not_264 : std_logic;
	signal or_266 : std_logic_vector(31 downto 0);
	signal and_267 : std_logic_vector(31 downto 0);
	signal or_272 : std_logic_vector(31 downto 0);
	signal and_273 : std_logic_vector(31 downto 0);
	signal or_280 : std_logic_vector(31 downto 0);
	signal and_281 : std_logic_vector(31 downto 0);
	signal or_287 : std_logic_vector(31 downto 0);
	signal and_288 : std_logic_vector(31 downto 0);
	signal or_293 : std_logic_vector(31 downto 0);
	signal and_294 : std_logic_vector(31 downto 0);
	signal or_301 : std_logic_vector(31 downto 0);
	signal and_302 : std_logic_vector(31 downto 0);
	signal or_304 : std_logic_vector(31 downto 0);
	signal and_305 : std_logic_vector(31 downto 0);
	signal or_306 : std_logic_vector(31 downto 0);
	signal and_307 : std_logic_vector(31 downto 0);
	signal or_308 : std_logic_vector(31 downto 0);
	signal and_309 : std_logic_vector(31 downto 0);
	signal or_312 : std_logic_vector(31 downto 0);
	signal and_313 : std_logic_vector(31 downto 0);
	signal or_318 : std_logic_vector(31 downto 0);
	signal and_319 : std_logic_vector(31 downto 0);
	signal or_329 : std_logic_vector(31 downto 0);
	signal and_330 : std_logic_vector(31 downto 0);
	signal or_335 : std_logic_vector(31 downto 0);
	signal and_336 : std_logic_vector(31 downto 0);
	signal or_339 : std_logic_vector(31 downto 0);
	signal and_340 : std_logic_vector(31 downto 0);
	signal or_342 : std_logic_vector(31 downto 0);
	signal and_343 : std_logic_vector(31 downto 0);
	signal or_346 : std_logic_vector(31 downto 0);
	signal and_347 : std_logic_vector(31 downto 0);
	signal or_348 : std_logic_vector(31 downto 0);
	signal and_349 : std_logic_vector(31 downto 0);
	signal or_351 : std_logic_vector(30 downto 0);
	signal and_352 : std_logic_vector(30 downto 0);
	signal or_355 : std_logic_vector(30 downto 0);
	signal and_356 : std_logic_vector(30 downto 0);
	signal or_360 : std_logic_vector(31 downto 0);
	signal and_361 : std_logic_vector(31 downto 0);
	signal or_371 : std_logic_vector(31 downto 0);
	signal and_372 : std_logic_vector(31 downto 0);
	signal or_378 : std_logic_vector(31 downto 0);
	signal and_379 : std_logic_vector(31 downto 0);
	signal or_380 : std_logic_vector(31 downto 0);
	signal and_381 : std_logic_vector(31 downto 0);
	signal or_384 : std_logic_vector(31 downto 0);
	signal and_385 : std_logic_vector(31 downto 0);
	signal or_386 : std_logic_vector(31 downto 0);
	signal and_387 : std_logic_vector(31 downto 0);
	signal or_388 : std_logic_vector(31 downto 0);
	signal and_389 : std_logic_vector(31 downto 0);
	signal or_394 : std_logic_vector(7 downto 0);
	signal and_395 : std_logic_vector(7 downto 0);
	signal and_397 : std_logic_vector(7 downto 0);
	signal and_399 : std_logic_vector(7 downto 0);
	signal and_401 : std_logic_vector(7 downto 0);
	signal or_414 : std_logic_vector(30 downto 0);
	signal or_423 : std_logic_vector(31 downto 0);
	signal and_424 : std_logic_vector(31 downto 0);
	signal or_425 : std_logic_vector(31 downto 0);
	signal and_426 : std_logic_vector(31 downto 0);
	signal or_427 : std_logic_vector(31 downto 0);
	signal and_428 : std_logic_vector(31 downto 0);
	signal or_431 : std_logic_vector(31 downto 0);
	signal and_432 : std_logic_vector(31 downto 0);
	signal or_433 : std_logic_vector(31 downto 0);
	signal and_434 : std_logic_vector(31 downto 0);
	signal or_438 : std_logic_vector(31 downto 0);
	signal and_439 : std_logic_vector(31 downto 0);
	signal or_440 : std_logic_vector(31 downto 0);
	signal and_441 : std_logic_vector(31 downto 0);
	signal or_443 : std_logic_vector(31 downto 0);
	signal and_444 : std_logic_vector(31 downto 0);
	signal or_450 : std_logic_vector(31 downto 0);
	signal and_451 : std_logic_vector(31 downto 0);
	signal or_454 : std_logic_vector(30 downto 0);
	signal and_455 : std_logic_vector(30 downto 0);
	signal or_458 : std_logic_vector(31 downto 0);
	signal and_459 : std_logic_vector(31 downto 0);
	signal or_462 : std_logic_vector(31 downto 0);
	signal and_463 : std_logic_vector(31 downto 0);
	signal or_467 : std_logic_vector(30 downto 0);
	signal and_468 : std_logic_vector(30 downto 0);
	signal or_475 : std_logic_vector(30 downto 0);
	signal and_476 : std_logic_vector(30 downto 0);
	signal or_479 : std_logic_vector(31 downto 0);
	signal and_480 : std_logic_vector(31 downto 0);
	signal or_481 : std_logic_vector(31 downto 0);
	signal and_482 : std_logic_vector(31 downto 0);
	signal or_485 : std_logic_vector(31 downto 0);
	signal and_486 : std_logic_vector(31 downto 0);
	signal or_490 : std_logic_vector(31 downto 0);
	signal and_491 : std_logic_vector(31 downto 0);
	signal or_493 : std_logic_vector(31 downto 0);
	signal or_497 : std_logic_vector(31 downto 0);
	signal or_512 : std_logic_vector(31 downto 0);
	signal and_513 : std_logic_vector(31 downto 0);
	signal or_518 : std_logic_vector(30 downto 0);
	signal and_519 : std_logic_vector(30 downto 0);
	signal or_525 : std_logic_vector(31 downto 0);
	signal and_526 : std_logic_vector(31 downto 0);
	signal or_529 : std_logic_vector(30 downto 0);
	signal and_530 : std_logic_vector(30 downto 0);
	signal or_532 : std_logic_vector(30 downto 0);
	signal and_533 : std_logic_vector(30 downto 0);
	signal or_535 : std_logic_vector(31 downto 0);
	signal and_536 : std_logic_vector(31 downto 0);
	signal or_538 : std_logic_vector(31 downto 0);
	signal and_539 : std_logic_vector(31 downto 0);
	signal or_541 : std_logic_vector(31 downto 0);
	signal and_542 : std_logic_vector(31 downto 0);
	signal or_545 : std_logic_vector(30 downto 0);
	signal and_546 : std_logic_vector(30 downto 0);
	signal or_548 : std_logic_vector(31 downto 0);
	signal and_549 : std_logic_vector(31 downto 0);
	signal or_554 : std_logic_vector(30 downto 0);
	signal and_555 : std_logic_vector(30 downto 0);
	signal or_557 : std_logic_vector(30 downto 0);
	signal and_558 : std_logic_vector(30 downto 0);
	signal or_568 : std_logic_vector(31 downto 0);
	signal and_569 : std_logic_vector(31 downto 0);
	signal or_571 : std_logic_vector(30 downto 0);
	signal and_572 : std_logic_vector(30 downto 0);
	signal or_575 : std_logic_vector(30 downto 0);
	signal and_576 : std_logic_vector(30 downto 0);
	signal or_590 : std_logic_vector(31 downto 0);
	signal and_591 : std_logic_vector(31 downto 0);
	signal or_597 : std_logic_vector(31 downto 0);
	signal and_598 : std_logic_vector(31 downto 0);
	signal or_603 : std_logic_vector(30 downto 0);
	signal and_604 : std_logic_vector(30 downto 0);

	-- This utility function is used for to generate concatenations of std_logic

	-- Little utility function to ease concatenation of an std_logic
	-- and explicitely return an std_logic_vector
	function repeat(N: natural; B: std_logic) return std_logic_vector is
		variable result: std_logic_vector(N-1 downto 0);
	begin
		result := (others => B);
		return result;
	end;

begin

	-- Instantiation of components

	output_split2_i : output_split2 port map (
		wa0_data => mux_141,
		wa0_addr => mux_142,
		ra0_data => sig_1159,
		ra0_addr => mux_143,
		wa0_en => mux_144,
		clk => sig_clock
	);

	output_split3_i : output_split3 port map (
		wa0_data => mux_137,
		wa0_addr => mux_138,
		ra0_data => sig_1158,
		ra0_addr => mux_139,
		wa0_en => mux_140,
		clk => sig_clock
	);

	sub_159_i : sub_159 port map (
		gt => sig_1157,
		result => sig_1156,
		in_a => idct_2d_r,
		in_b => "00000000000000000000000011111111",
		sign => '1'
	);

	add_165_i : add_165 port map (
		result => sig_1155,
		in_a => idct_2d_yc_reg7(31 downto 5),
		in_b => "000000000000000000000000001"
	);

	output_split1_i : output_split1 port map (
		wa0_data => mux_89,
		wa0_addr => mux_90,
		ra0_data => sig_1154,
		ra0_addr => mux_91,
		wa0_en => mux_92,
		clk => sig_clock
	);

	output_split0_i : output_split0 port map (
		wa0_data => mux_85,
		wa0_addr => mux_86,
		ra0_data => sig_1153,
		ra0_addr => mux_87,
		wa0_en => mux_88,
		clk => sig_clock
	);

	add_172_i : add_172 port map (
		result => sig_1152,
		in_a => sig_1183,
		in_b => "00000000000000000001"
	);

	add_176_i : add_176 port map (
		result => sig_1151,
		in_a => sig_1182,
		in_b => "00000000000000000001"
	);

	add_181_i : add_181 port map (
		result => sig_1150,
		in_a => idct_z2_reg0,
		in_b => idct_z3_reg7
	);

	sub_187_i : sub_187 port map (
		result => sig_1149,
		in_a => idct_z2_reg1,
		in_b => idct_z3_reg6
	);

	mul_189_i : mul_189 port map (
		result => sig_1148,
		in_a => idct_z2_reg4(30 downto 0),
		in_b => "01000111000111"
	);

	add_191_i : add_191 port map (
		result => sig_1147,
		in_a => sig_1148,
		in_b => sig_1123(31 downto 1)
	);

	mul_192_i : mul_192 port map (
		result => sig_1146,
		in_a => idct_z2_reg5(29 downto 0),
		in_b => "01100011111"
	);

	mul_193_i : mul_193 port map (
		result => sig_1145,
		in_a => idct_z2_reg6,
		in_b => "011111011000101"
	);

	mul_198_i : mul_198 port map (
		result => sig_1144,
		in_a => idct_z2_reg4,
		in_b => "011010100110111"
	);

	mul_199_i : mul_199 port map (
		result => sig_1143,
		in_a => idct_z2_reg7(30 downto 0),
		in_b => "01000111000111"
	);

	sub_209_i : sub_209 port map (
		result => sig_1142,
		in_a => idct_2d_yc_reg1,
		in_b => idct_2d_yc_reg7
	);

	add_212_i : add_212 port map (
		result => sig_1141,
		in_a => idct_z1_reg1,
		in_b => idct_z1_reg2
	);

	sub_213_i : sub_213 port map (
		result => sig_1140,
		in_a => idct_z1_reg1,
		in_b => idct_z1_reg2
	);

	sub_214_i : sub_214 port map (
		result => sig_1139,
		in_a => idct_z1_reg0,
		in_b => idct_z1_reg3
	);

	mul_215_i : mul_215 port map (
		result => sig_1138,
		in_a => idct_2d_yc_reg2,
		in_b => "0101001110011111"
	);

	mul_216_i : mul_216 port map (
		result => sig_1137,
		in_a => idct_2d_yc_reg6,
		in_b => "010001010100011"
	);

	sub_217_i : sub_217 port map (
		result => sig_1136,
		in_a => sig_1138,
		in_b => sig_1137
	);

	mul_218_i : mul_218 port map (
		result => sig_1135,
		in_a => idct_2d_yc_reg2,
		in_b => "010001010100011"
	);

	mul_219_i : mul_219 port map (
		result => sig_1134,
		in_a => idct_2d_yc_reg6,
		in_b => "0101001110011111"
	);

	sub_220_i : sub_220 port map (
		result => sig_1133,
		in_a => sig_1135,
		in_b => sig_1134
	);

	mul_223_i : mul_223 port map (
		result => sig_1132,
		in_a => idct_2d_yc_reg5(30 downto 0),
		in_b => "010110101000001"
	);

	sub_227_i : sub_227 port map (
		result => sig_1131,
		in_a => idct_2d_yc_reg0,
		in_b => idct_2d_yc_reg4
	);

	sub_157_i : sub_157 port map (
		ge => sig_1130,
		result => sig_1129,
		in_a => idct_2d_r,
		in_b => "00000000000000000000000000000000",
		sign => '1'
	);

	add_163_i : add_163 port map (
		result => sig_1128,
		in_a => psc_loop_reg_13,
		in_b => "0000000000000001"
	);

	cmp_164_i : cmp_164 port map (
		ne => memextrct_loop_sig_21,
		in0 => "0000000000011111",
		in1 => psc_loop_reg_13
	);

	add_170_i : add_170 port map (
		result => sig_1127,
		in_a => sig_1181,
		in_b => "00000000000000000001"
	);

	add_174_i : add_174 port map (
		result => sig_1126,
		in_a => sig_1180,
		in_b => "00000000000000000001"
	);

	add_180_i : add_180 port map (
		result => sig_1125,
		in_a => sig_1179,
		in_b => "00000000000000000001"
	);

	sub_186_i : sub_186 port map (
		result => sig_1124,
		in_a => idct_z2_reg2,
		in_b => idct_z3_reg5
	);

	mul_190_i : mul_190 port map (
		result => sig_1123,
		in_a => idct_z2_reg7,
		in_b => "011010100110111"
	);

	mul_196_i : mul_196 port map (
		result => sig_1122,
		in_a => idct_z2_reg6(29 downto 0),
		in_b => "01100011111"
	);

	sub_200_i : sub_200 port map (
		result => sig_1121,
		in_a => sig_1144,
		in_b => sig_1178
	);

	add_206_i : add_206 port map (
		result => sig_1120,
		in_a => idct_z1_reg4,
		in_b => idct_z1_reg6
	);

	add_210_i : add_210 port map (
		result => sig_1119,
		in_a => idct_2d_yc_reg1,
		in_b => idct_2d_yc_reg7
	);

	add_171_i : add_171 port map (
		result => sig_1118,
		in_a => idct_2d_yc_reg4(31 downto 5),
		in_b => "000000000000000000000000001"
	);

	add_177_i : add_177 port map (
		result => sig_1117,
		in_a => idct_2d_yc_reg1(31 downto 5),
		in_b => "000000000000000000000000001"
	);

	add_179_i : add_179 port map (
		result => sig_1116,
		in_a => idct_2d_yc_reg0(31 downto 5),
		in_b => "000000000000000000000000001"
	);

	mul_195_i : mul_195 port map (
		result => sig_1115,
		in_a => idct_z2_reg5,
		in_b => "011111011000101"
	);

	sub_197_i : sub_197 port map (
		result => sig_1114,
		in_a => sig_1115,
		in_b => sig_1177
	);

	sub_207_i : sub_207 port map (
		result => sig_1113,
		in_a => idct_z1_reg7,
		in_b => idct_z1_reg5
	);

	mul_230_i : mul_230 port map (
		result => sig_1112,
		in_a => idct_2d_yc_reg3(30 downto 0),
		in_b => "010110101000001"
	);

	sub_185_i : sub_185 port map (
		result => sig_1111,
		in_a => idct_z2_reg3,
		in_b => idct_z3_reg4
	);

	add_211_i : add_211 port map (
		result => sig_1110,
		in_a => idct_z1_reg0,
		in_b => idct_z1_reg3
	);

	add_226_i : add_226 port map (
		result => sig_1109,
		in_a => idct_2d_yc_reg0,
		in_b => idct_2d_yc_reg4
	);

	add_235_i : add_235 port map (
		result => sig_1108,
		in_a => idct_2d_yc_reg2(31 downto 5),
		in_b => "000000000000000000000000001"
	);

	add_314_i : add_314 port map (
		result => sig_1107,
		in_a => idct_2d_yc_reg2(31 downto 5),
		in_b => "000000000000000000000000001"
	);

	sub_160_i : sub_160 port map (
		le => sig_1106,
		result => sig_1105,
		in_a => idct_2d_r,
		in_b => "00000000000000000000000011111111",
		sign => '1'
	);

	add_173_i : add_173 port map (
		result => sig_1104,
		in_a => idct_2d_yc_reg3(31 downto 5),
		in_b => "000000000000000000000000001"
	);

	add_182_i : add_182 port map (
		result => sig_1103,
		in_a => idct_z2_reg1,
		in_b => idct_z3_reg6
	);

	sub_188_i : sub_188 port map (
		result => sig_1102,
		in_a => idct_z2_reg0,
		in_b => idct_z3_reg7
	);

	sub_243_i : sub_243 port map (
		result => sig_1101,
		in_a => sig_1115,
		in_b => sig_1176
	);

	sub_262_i : sub_262 port map (
		result => sig_1100,
		in_a => sig_1115,
		in_b => sig_1175
	);

	output_split4_i : output_split4 port map (
		wa0_data => mux_65,
		wa0_addr => mux_66,
		ra0_data => sig_1099,
		ra0_addr => mux_67,
		wa0_en => mux_68,
		clk => sig_clock
	);

	output_split5_i : output_split5 port map (
		wa0_data => mux_61,
		wa0_addr => mux_62,
		ra0_data => sig_1098,
		ra0_addr => mux_63,
		wa0_en => mux_64,
		clk => sig_clock
	);

	output_split6_i : output_split6 port map (
		wa0_data => mux_57,
		wa0_addr => mux_58,
		ra0_data => sig_1097,
		ra0_addr => mux_59,
		wa0_en => mux_60,
		clk => sig_clock
	);

	output_split7_i : output_split7 port map (
		wa0_data => mux_53,
		wa0_addr => mux_54,
		ra0_data => sig_1096,
		ra0_addr => mux_55,
		wa0_en => mux_56,
		clk => sig_clock
	);

	input_split0_i : input_split0 port map (
		ra0_data => sig_1095,
		ra0_addr => mux_46,
		ra1_data => sig_1094,
		ra1_addr => mux_47,
		ra2_data => sig_1093,
		ra2_addr => mux_48,
		ra3_data => sig_1092,
		ra3_addr => mux_49,
		clk => sig_clock,
		wa2_data => mux_50,
		wa2_addr => mux_51,
		wa2_en => mux_52
	);

	add_194_i : add_194 port map (
		result => sig_1091,
		in_a => sig_1146,
		in_b => sig_1145(31 downto 2)
	);

	add_205_i : add_205 port map (
		result => sig_1090,
		in_a => idct_z1_reg7,
		in_b => idct_z1_reg5
	);

	add_254_i : add_254 port map (
		result => sig_1089,
		in_a => idct_2d_yc_reg2(31 downto 5),
		in_b => "000000000000000000000000001"
	);

	add_276_i : add_276 port map (
		result => sig_1088,
		in_a => idct_2d_yc_reg2(31 downto 5),
		in_b => "000000000000000000000000001"
	);

	sub_284_i : sub_284 port map (
		result => sig_1087,
		in_a => sig_1115,
		in_b => sig_1174
	);

	input_split1_i : input_split1 port map (
		wa0_data => mux_39,
		wa0_addr => mux_40,
		ra0_data => sig_1086,
		ra0_addr => mux_41,
		wa0_en => mux_42,
		ra1_data => sig_1085,
		ra1_addr => mux_43,
		ra2_data => sig_1084,
		ra2_addr => mux_44,
		ra3_data => sig_1083,
		ra3_addr => mux_45,
		clk => sig_clock
	);

	add_166_i : add_166 port map (
		result => sig_1082,
		in_a => sig_1173,
		in_b => "00000000000000000001"
	);

	add_168_i : add_168 port map (
		result => sig_1081,
		in_a => sig_1172,
		in_b => "00000000000000000001"
	);

	add_178_i : add_178 port map (
		result => sig_1080,
		in_a => sig_1171,
		in_b => "00000000000000000001"
	);

	add_183_i : add_183 port map (
		result => sig_1079,
		in_a => idct_z2_reg2,
		in_b => idct_z3_reg5
	);

	sub_332_i : sub_332 port map (
		result => sig_1078,
		in_a => sig_689,
		in_b => sig_688
	);

	mul_341_i : mul_341 port map (
		result => sig_1077,
		in_a => or_339,
		in_b => "0101001110011111"
	);

	mul_357_i : mul_357 port map (
		result => sig_1076,
		in_a => or_355,
		in_b => "010110101000001"
	);

	mul_365_i : mul_365 port map (
		result => sig_1075,
		in_a => or_363,
		in_b => "010001010100011"
	);

	mul_368_i : mul_368 port map (
		result => sig_1074,
		in_a => or_366,
		in_b => "0101001110011111"
	);

	sub_369_i : sub_369 port map (
		result => sig_1073,
		in_a => sig_1075,
		in_b => sig_1074
	);

	sub_370_i : sub_370 port map (
		result => sig_1072,
		in_a => sig_1115,
		in_b => sig_1170
	);

	sub_377_i : sub_377 port map (
		result => sig_1071,
		in_a => sig_680,
		in_b => sig_715
	);

	cmp_398_i : cmp_398 port map (
		eq => sig_1070,
		in0 => "110",
		in1 => augh_main_k(2 downto 0)
	);

	cmp_400_i : cmp_400 port map (
		eq => sig_1069,
		in0 => "101",
		in1 => augh_main_k(2 downto 0)
	);

	cmp_404_i : cmp_404 port map (
		eq => sig_1068,
		in0 => "011",
		in1 => augh_main_k(2 downto 0)
	);

	cmp_406_i : cmp_406 port map (
		eq => sig_1067,
		in0 => "010",
		in1 => augh_main_k(2 downto 0)
	);

	cmp_408_i : cmp_408 port map (
		eq => sig_1066,
		in0 => "001",
		in1 => augh_main_k(2 downto 0)
	);

	cmp_410_i : cmp_410 port map (
		eq => sig_1065,
		in0 => "000",
		in1 => augh_main_k(2 downto 0)
	);

	cmp_412_i : cmp_412 port map (
		eq => sig_1064,
		in0 => '0',
		in1 => augh_main_k(0)
	);

	sub_429_i : sub_429 port map (
		result => sig_1063,
		in_a => or_421,
		in_b => or_427
	);

	add_466_i : add_466 port map (
		result => sig_1062,
		in_a => or_462,
		in_b => or_464
	);

	sub_496_i : sub_496 port map (
		result => sig_1061,
		in_a => sig_652,
		in_b => sig_651
	);

	sub_521_i : sub_521 port map (
		result => sig_1060,
		in_a => or_438,
		in_b => or_464
	);

	sub_528_i : sub_528 port map (
		result => sig_1059,
		in_a => sig_643,
		in_b => sig_642
	);

	fsm_23_i : fsm_23 port map (
		clock => sig_clock,
		reset => sig_reset,
		in0 => memextrct_loop_sig_21,
		out181 => sig_1058,
		out182 => sig_1057,
		out183 => sig_1056,
		out184 => sig_1055,
		out185 => sig_1054,
		out8 => sig_1053,
		out13 => sig_1052,
		out14 => sig_1051,
		out16 => sig_1050,
		out18 => sig_1049,
		out19 => sig_1048,
		out20 => sig_1047,
		out21 => sig_1046,
		out22 => sig_1045,
		in2 => sig_start,
		out23 => sig_1044,
		out24 => sig_1043,
		out25 => sig_1042,
		out26 => sig_1041,
		out27 => sig_1040,
		out28 => sig_1039,
		out29 => sig_1038,
		out30 => sig_1037,
		out31 => sig_1036,
		out33 => sig_1035,
		out35 => sig_1034,
		out36 => sig_1033,
		out38 => sig_1032,
		out40 => sig_1031,
		out42 => sig_1030,
		in3 => memextrct_loop_sig_22,
		out44 => sig_1029,
		out46 => sig_1028,
		out48 => sig_1027,
		out49 => sig_1026,
		out50 => sig_1025,
		out52 => sig_1024,
		out54 => sig_1023,
		out56 => sig_1022,
		out57 => sig_1021,
		out58 => sig_1020,
		in4 => test_cp_0_16,
		out60 => sig_1019,
		in5 => test_cp_1_17,
		out164 => sig_1018,
		out165 => sig_1017,
		out167 => sig_1016,
		out168 => sig_1015,
		out170 => sig_1014,
		out171 => sig_1013,
		out173 => sig_1012,
		out174 => sig_1011,
		out176 => sig_1010,
		out178 => sig_1009,
		out0 => sig_1008,
		out1 => sig_1007,
		out2 => sig_1006,
		in1 => cp_rest,
		out4 => sig_1005,
		out90 => sig_1004,
		out91 => sig_1003,
		out97 => sig_1002,
		out99 => sig_1001,
		out101 => sig_1000,
		in6 => stdout_ack,
		out103 => sig_999,
		out105 => sig_998,
		out106 => sig_997,
		out107 => sig_996,
		out108 => sig_995,
		out135 => sig_994,
		out136 => sig_993,
		out137 => sig_992,
		out138 => sig_991,
		in11 => augh_test_9,
		out140 => sig_990,
		out141 => sig_989,
		out142 => sig_988,
		out143 => sig_987,
		out145 => sig_986,
		out146 => sig_985,
		out148 => sig_984,
		out150 => sig_983,
		out153 => sig_982,
		out154 => sig_981,
		out155 => sig_980,
		out156 => sig_979,
		out157 => sig_978,
		out158 => sig_977,
		out159 => sig_976,
		out160 => sig_975,
		out161 => sig_974,
		out162 => sig_973,
		out111 => sig_972,
		out112 => sig_971,
		out114 => sig_970,
		out116 => sig_969,
		out118 => sig_968,
		out120 => sig_967,
		out121 => sig_966,
		out122 => sig_965,
		out123 => sig_964,
		out124 => sig_963,
		out125 => sig_962,
		out126 => sig_961,
		in7 => cp_en,
		out129 => sig_960,
		out130 => sig_959,
		in8 => stdin_ack,
		out131 => sig_958,
		in9 => psc_loop_sig_20,
		out132 => sig_957,
		out133 => sig_956,
		out134 => sig_955,
		in10 => augh_test_11,
		out186 => sig_954,
		out187 => sig_953,
		out190 => sig_952,
		out195 => sig_951,
		out197 => sig_950,
		out198 => sig_949,
		out199 => sig_948,
		out200 => sig_947,
		out201 => sig_946,
		out203 => sig_945,
		out204 => sig_944,
		out206 => sig_943,
		out207 => sig_942,
		out209 => sig_941,
		out210 => sig_940,
		out212 => sig_939,
		out213 => sig_938,
		out215 => sig_937,
		out217 => sig_936,
		out220 => sig_935,
		out221 => sig_934,
		out222 => sig_933,
		out223 => sig_932,
		out224 => sig_931,
		out225 => sig_930,
		out226 => sig_929,
		out227 => sig_928,
		out228 => sig_927,
		out229 => sig_926,
		out231 => sig_925,
		out232 => sig_924,
		out234 => sig_923,
		out235 => sig_922,
		out237 => sig_921,
		out238 => sig_920,
		out240 => sig_919,
		out241 => sig_918,
		out243 => sig_917,
		out245 => sig_916,
		out248 => sig_915,
		out249 => sig_914,
		out250 => sig_913,
		out251 => sig_912,
		out252 => sig_911,
		out253 => sig_910,
		out254 => sig_909,
		out255 => sig_908,
		out256 => sig_907,
		out257 => sig_906,
		out259 => sig_905,
		out260 => sig_904,
		out262 => sig_903,
		out263 => sig_902,
		out265 => sig_901,
		out266 => sig_900,
		out268 => sig_899,
		out269 => sig_898,
		out271 => sig_897,
		out273 => sig_896,
		out276 => sig_895,
		out277 => sig_894,
		out278 => sig_893,
		out279 => sig_892,
		out280 => sig_891,
		out281 => sig_890,
		out282 => sig_889,
		out283 => sig_888,
		out284 => sig_887,
		out285 => sig_886,
		out286 => sig_885,
		out287 => sig_884,
		out288 => sig_883,
		out289 => sig_882,
		out290 => sig_881,
		out291 => sig_880,
		out292 => sig_879,
		out293 => sig_878,
		out294 => sig_877,
		out295 => sig_876,
		out296 => sig_875,
		out297 => sig_874,
		out298 => sig_873,
		out311 => sig_872,
		out312 => sig_871,
		out313 => sig_870,
		out314 => sig_869,
		out315 => sig_868,
		out316 => sig_867,
		out318 => sig_866,
		out321 => sig_865,
		out322 => sig_864,
		out323 => sig_863,
		out324 => sig_862,
		out325 => sig_861,
		out326 => sig_860,
		out327 => sig_859,
		out328 => sig_858,
		out329 => sig_857,
		out333 => sig_856,
		out341 => sig_855,
		out342 => sig_854,
		out343 => sig_853,
		out344 => sig_852,
		out345 => sig_851,
		out346 => sig_850,
		out349 => sig_849,
		out350 => sig_848,
		out351 => sig_847,
		out352 => sig_846,
		out353 => sig_845,
		out354 => sig_844,
		out355 => sig_843,
		out357 => sig_842,
		out361 => sig_841,
		out362 => sig_840,
		out363 => sig_839,
		out364 => sig_838,
		out366 => sig_837,
		out367 => sig_836,
		out371 => sig_835,
		out372 => sig_834,
		out373 => sig_833,
		out382 => sig_832,
		out383 => sig_831,
		out385 => sig_830,
		out393 => sig_829,
		out394 => sig_828,
		out395 => sig_827,
		out396 => sig_826,
		out398 => sig_825,
		out400 => sig_824,
		out401 => sig_823,
		out402 => sig_822,
		out404 => sig_821,
		out406 => sig_820,
		out407 => sig_819,
		out408 => sig_818,
		out409 => sig_817,
		out410 => sig_816,
		out411 => sig_815,
		out412 => sig_814,
		out413 => sig_813,
		out414 => sig_812,
		out416 => sig_811,
		out417 => sig_810,
		out418 => sig_809,
		out419 => sig_808,
		out422 => sig_807,
		out423 => sig_806,
		out425 => sig_805,
		out426 => sig_804,
		out428 => sig_803,
		out429 => sig_802,
		out430 => sig_801,
		out431 => sig_800,
		out433 => sig_799,
		out434 => sig_798,
		out435 => sig_797,
		out436 => sig_796,
		out437 => sig_795,
		out438 => sig_794,
		out440 => sig_793,
		out441 => sig_792,
		out443 => sig_791,
		out444 => sig_790,
		out445 => sig_789,
		out446 => sig_788,
		out447 => sig_787,
		out450 => sig_786,
		out451 => sig_785,
		out454 => sig_784,
		out455 => sig_783,
		out457 => sig_782,
		out458 => sig_781,
		out459 => sig_780,
		out460 => sig_779,
		out461 => sig_778,
		out462 => sig_777,
		out463 => sig_776,
		out464 => sig_775,
		out465 => sig_774,
		out466 => sig_773,
		out467 => sig_772,
		out468 => sig_771,
		out469 => sig_770,
		out472 => sig_769,
		out475 => sig_768,
		out481 => sig_767,
		out482 => sig_766,
		out483 => sig_765,
		out484 => sig_764,
		out487 => sig_763,
		out488 => sig_762,
		out491 => sig_761,
		out495 => sig_760,
		out496 => sig_759,
		out497 => sig_758,
		out498 => sig_757,
		out499 => sig_756,
		out500 => sig_755,
		out501 => sig_754,
		out512 => sig_753,
		out513 => sig_752,
		out517 => sig_751,
		out518 => sig_750,
		out519 => sig_749,
		out521 => sig_748,
		out522 => sig_747,
		out524 => sig_746,
		out525 => sig_745,
		out526 => sig_744,
		out527 => sig_743,
		out528 => sig_742,
		out531 => sig_741,
		out540 => sig_740,
		out542 => sig_739,
		out544 => sig_738,
		out545 => sig_737,
		out554 => sig_736,
		out555 => sig_735,
		out559 => sig_734,
		out560 => sig_733,
		out561 => sig_732,
		out562 => sig_731,
		out563 => sig_730,
		out566 => sig_729,
		out567 => sig_728,
		out570 => sig_727,
		out572 => sig_726,
		out575 => sig_725,
		out577 => sig_724,
		out578 => sig_723,
		out580 => sig_722,
		out581 => sig_721
	);

	add_167_i : add_167 port map (
		result => sig_720,
		in_a => idct_2d_yc_reg6(31 downto 5),
		in_b => "000000000000000000000000001"
	);

	add_169_i : add_169 port map (
		result => sig_719,
		in_a => idct_2d_yc_reg5(31 downto 5),
		in_b => "000000000000000000000000001"
	);

	add_175_i : add_175 port map (
		result => sig_718,
		in_a => idct_2d_yc_reg2(31 downto 5),
		in_b => "000000000000000000000000001"
	);

	add_255_i : add_255 port map (
		result => sig_717,
		in_a => sig_1169,
		in_b => "00000000000000000001"
	);

	sub_362_i : sub_362 port map (
		result => sig_716,
		in_a => or_358,
		in_b => or_360
	);

	mul_376_i : mul_376 port map (
		result => sig_715,
		in_a => or_374,
		in_b => "010001010100011"
	);

	add_420_i : add_420 port map (
		result => sig_714,
		in_a => sig_1168,
		in_b => "00000000000000000001"
	);

	sub_446_i : sub_446 port map (
		result => sig_713,
		in_a => sig_667,
		in_b => sig_666
	);

	mul_456_i : mul_456 port map (
		result => sig_712,
		in_a => or_454,
		in_b => "010110101000001"
	);

	mul_457_i : mul_457 port map (
		result => sig_711,
		in_a => or_450,
		in_b => "0101001110011111"
	);

	sub_461_i : sub_461 port map (
		result => sig_710,
		in_a => sig_711,
		in_b => sig_662
	);

	sub_517_i : sub_517 port map (
		result => sig_709,
		in_a => or_512,
		in_b => or_514
	);

	mul_560_i : mul_560 port map (
		result => sig_708,
		in_a => or_435,
		in_b => "010001010100011"
	);

	mul_565_i : mul_565 port map (
		result => sig_707,
		in_a => or_363,
		in_b => "0101001110011111"
	);

	mul_578_i : mul_578 port map (
		result => sig_706,
		in_a => or_431,
		in_b => "010001010100011"
	);

	muxb_162_i : muxb_162 port map (
		in_sel => cp_en,
		out_data => sig_705,
		in_data0 => '0',
		in_data1 => '1'
	);

	add_184_i : add_184 port map (
		result => sig_704,
		in_a => idct_z2_reg3,
		in_b => idct_z3_reg4
	);

	muxb_201_i : muxb_201 port map (
		in_sel => cp_en,
		out_data => sig_703,
		in_data0 => '0',
		in_data1 => '1'
	);

	cmp_202_i : cmp_202 port map (
		ne => memextrct_loop_sig_22,
		in0 => "0000000000000111",
		in1 => psc_loop_reg_13
	);

	cmp_203_i : cmp_203 port map (
		eq => test_cp_1_17,
		in0 => '1',
		in1 => cp_id_reg_stable_15
	);

	cmp_204_i : cmp_204 port map (
		eq => sig_702,
		in0 => '0',
		in1 => cp_id_reg_stable_15
	);

	sub_208_i : sub_208 port map (
		result => sig_701,
		in_a => idct_z1_reg4,
		in_b => idct_z1_reg6
	);

	add_236_i : add_236 port map (
		result => sig_700,
		in_a => sig_1167,
		in_b => "00000000000000000001"
	);

	muxb_263_i : muxb_263 port map (
		in_sel => not_264,
		out_data => sig_699,
		in_data0 => '0',
		in_data1 => '1'
	);

	muxb_265_i : muxb_265 port map (
		in_sel => not_264,
		out_data => sig_698,
		in_data0 => '0',
		in_data1 => '1'
	);

	add_277_i : add_277 port map (
		result => sig_697,
		in_a => sig_1166,
		in_b => "00000000000000000001"
	);

	add_295_i : add_295 port map (
		result => sig_696,
		in_a => idct_2d_yc_reg2(31 downto 5),
		in_b => "000000000000000000000000001"
	);

	add_296_i : add_296 port map (
		result => sig_695,
		in_a => sig_1165,
		in_b => "00000000000000000001"
	);

	sub_303_i : sub_303 port map (
		result => sig_694,
		in_a => sig_1115,
		in_b => sig_1164
	);

	add_315_i : add_315 port map (
		result => sig_693,
		in_a => sig_1163,
		in_b => "00000000000000000001"
	);

	muxb_322_i : muxb_322 port map (
		in_sel => cp_en,
		out_data => sig_692,
		in_data0 => '0',
		in_data1 => '1'
	);

	add_323_i : add_323 port map (
		result => sig_691,
		in_a => psc_loop_reg_13,
		in_b => "0000000000000001"
	);

	cmp_324_i : cmp_324 port map (
		ne => psc_loop_sig_20,
		in0 => "0000000000000001",
		in1 => psc_loop_reg_13
	);

	cmp_325_i : cmp_325 port map (
		eq => sig_690,
		in0 => '0',
		in1 => cp_id_reg_stable_15
	);

	mul_328_i : mul_328 port map (
		result => sig_689,
		in_a => or_326,
		in_b => "010001010100011"
	);

	mul_331_i : mul_331 port map (
		result => sig_688,
		in_a => or_329,
		in_b => "0101001110011111"
	);

	sub_337_i : sub_337 port map (
		result => sig_687,
		in_a => or_333,
		in_b => or_335
	);

	add_338_i : add_338 port map (
		result => sig_686,
		in_a => or_333,
		in_b => or_297
	);

	mul_344_i : mul_344 port map (
		result => sig_685,
		in_a => or_342,
		in_b => "010001010100011"
	);

	sub_345_i : sub_345 port map (
		result => sig_684,
		in_a => sig_1077,
		in_b => sig_685
	);

	add_350_i : add_350 port map (
		result => sig_683,
		in_a => or_346,
		in_b => or_348
	);

	mul_353_i : mul_353 port map (
		result => sig_682,
		in_a => or_351,
		in_b => "010110101000001"
	);

	sub_354_i : sub_354 port map (
		result => sig_681,
		in_a => or_346,
		in_b => or_348
	);

	mul_373_i : mul_373 port map (
		result => sig_680,
		in_a => or_371,
		in_b => "0101001110011111"
	);

	add_382_i : add_382 port map (
		result => sig_679,
		in_a => or_378,
		in_b => or_380
	);

	mul_383_i : mul_383 port map (
		result => sig_678,
		in_a => idct_2d_yc_reg3(30 downto 0),
		in_b => "010110101000001"
	);

	add_390_i : add_390 port map (
		result => sig_677,
		in_a => or_386,
		in_b => or_388
	);

	sub_391_i : sub_391 port map (
		result => sig_676,
		in_a => or_386,
		in_b => or_388
	);

	cmp_392_i : cmp_392 port map (
		ne => augh_test_11,
		in0 => "00000000000000000000000000111111",
		in1 => augh_main_k
	);

	add_393_i : add_393 port map (
		result => sig_675,
		in_a => augh_main_k,
		in_b => "00000000000000000000000000000001"
	);

	cmp_396_i : cmp_396 port map (
		eq => sig_674,
		in0 => "111",
		in1 => augh_main_k(2 downto 0)
	);

	cmp_402_i : cmp_402 port map (
		eq => sig_673,
		in0 => "100",
		in1 => augh_main_k(2 downto 0)
	);

	cmp_411_i : cmp_411 port map (
		eq => sig_672,
		in0 => '1',
		in1 => augh_main_k(0)
	);

	cmp_413_i : cmp_413 port map (
		ne => augh_test_9,
		in0 => "00000000000000000000000000111111",
		in1 => augh_main_k
	);

	mul_416_i : mul_416 port map (
		result => sig_671,
		in_a => or_414,
		in_b => "010110101000001"
	);

	add_419_i : add_419 port map (
		result => sig_670,
		in_a => idct_2d_yc_reg2(31 downto 5),
		in_b => "000000000000000000000000001"
	);

	add_430_i : add_430 port map (
		result => sig_669,
		in_a => or_421,
		in_b => or_427
	);

	sub_437_i : sub_437 port map (
		result => sig_668,
		in_a => sig_1115,
		in_b => sig_1162
	);

	mul_442_i : mul_442 port map (
		result => sig_667,
		in_a => or_440,
		in_b => "0101001110011111"
	);

	mul_445_i : mul_445 port map (
		result => sig_666,
		in_a => or_443,
		in_b => "010001010100011"
	);

	mul_447_i : mul_447 port map (
		result => sig_665,
		in_a => or_440,
		in_b => "010001010100011"
	);

	mul_448_i : mul_448 port map (
		result => sig_664,
		in_a => or_443,
		in_b => "0101001110011111"
	);

	sub_449_i : sub_449 port map (
		result => sig_663,
		in_a => sig_665,
		in_b => sig_664
	);

	mul_460_i : mul_460 port map (
		result => sig_662,
		in_a => or_458,
		in_b => "010001010100011"
	);

	mul_469_i : mul_469 port map (
		result => sig_661,
		in_a => or_467,
		in_b => "010110101000001"
	);

	add_474_i : add_474 port map (
		result => sig_660,
		in_a => or_470,
		in_b => or_472
	);

	mul_477_i : mul_477 port map (
		result => sig_659,
		in_a => or_475,
		in_b => "010110101000001"
	);

	sub_478_i : sub_478 port map (
		result => sig_658,
		in_a => or_470,
		in_b => or_472
	);

	add_483_i : add_483 port map (
		result => sig_657,
		in_a => or_479,
		in_b => or_481
	);

	sub_484_i : sub_484 port map (
		result => sig_656,
		in_a => or_479,
		in_b => or_481
	);

	add_487_i : add_487 port map (
		result => sig_655,
		in_a => or_425,
		in_b => or_485
	);

	sub_488_i : sub_488 port map (
		result => sig_654,
		in_a => or_425,
		in_b => or_485
	);

	sub_489_i : sub_489 port map (
		result => sig_653,
		in_a => or_378,
		in_b => or_285
	);

	mul_492_i : mul_492 port map (
		result => sig_652,
		in_a => or_490,
		in_b => "010001010100011"
	);

	mul_495_i : mul_495 port map (
		result => sig_651,
		in_a => or_493,
		in_b => "0101001110011111"
	);

	mul_499_i : mul_499 port map (
		result => sig_650,
		in_a => or_435,
		in_b => "0101001110011111"
	);

	mul_502_i : mul_502 port map (
		result => sig_649,
		in_a => or_500,
		in_b => "010001010100011"
	);

	sub_503_i : sub_503 port map (
		result => sig_648,
		in_a => sig_650,
		in_b => sig_649
	);

	add_508_i : add_508 port map (
		result => sig_647,
		in_a => or_504,
		in_b => or_506
	);

	mul_511_i : mul_511 port map (
		result => sig_646,
		in_a => or_509,
		in_b => "010110101000001"
	);

	add_516_i : add_516 port map (
		result => sig_645,
		in_a => or_512,
		in_b => or_514
	);

	mul_520_i : mul_520 port map (
		result => sig_644,
		in_a => or_518,
		in_b => "010110101000001"
	);

	mul_524_i : mul_524 port map (
		result => sig_643,
		in_a => or_522,
		in_b => "010001010100011"
	);

	mul_527_i : mul_527 port map (
		result => sig_642,
		in_a => or_525,
		in_b => "0101001110011111"
	);

	mul_531_i : mul_531 port map (
		result => sig_641,
		in_a => or_529,
		in_b => "010110101000001"
	);

	mul_534_i : mul_534 port map (
		result => sig_640,
		in_a => or_532,
		in_b => "010110101000001"
	);

	add_537_i : add_537 port map (
		result => sig_639,
		in_a => or_497,
		in_b => or_535
	);

	mul_540_i : mul_540 port map (
		result => sig_638,
		in_a => or_538,
		in_b => "0101001110011111"
	);

	mul_543_i : mul_543 port map (
		result => sig_637,
		in_a => or_541,
		in_b => "010001010100011"
	);

	sub_544_i : sub_544 port map (
		result => sig_636,
		in_a => sig_638,
		in_b => sig_637
	);

	mul_547_i : mul_547 port map (
		result => sig_635,
		in_a => or_545,
		in_b => "010110101000001"
	);

	add_552_i : add_552 port map (
		result => sig_634,
		in_a => or_548,
		in_b => or_550
	);

	sub_553_i : sub_553 port map (
		result => sig_633,
		in_a => or_548,
		in_b => or_550
	);

	mul_556_i : mul_556 port map (
		result => sig_632,
		in_a => or_554,
		in_b => "010110101000001"
	);

	mul_559_i : mul_559 port map (
		result => sig_631,
		in_a => or_557,
		in_b => "010110101000001"
	);

	mul_561_i : mul_561 port map (
		result => sig_630,
		in_a => or_500,
		in_b => "0101001110011111"
	);

	sub_562_i : sub_562 port map (
		result => sig_629,
		in_a => sig_708,
		in_b => sig_630
	);

	sub_563_i : sub_563 port map (
		result => sig_628,
		in_a => or_504,
		in_b => or_506
	);

	add_564_i : add_564 port map (
		result => sig_627,
		in_a => or_358,
		in_b => or_360
	);

	mul_566_i : mul_566 port map (
		result => sig_626,
		in_a => or_366,
		in_b => "010001010100011"
	);

	sub_567_i : sub_567 port map (
		result => sig_625,
		in_a => sig_707,
		in_b => sig_626
	);

	add_570_i : add_570 port map (
		result => sig_624,
		in_a => or_417,
		in_b => or_568
	);

	mul_573_i : mul_573 port map (
		result => sig_623,
		in_a => or_571,
		in_b => "010110101000001"
	);

	sub_574_i : sub_574 port map (
		result => sig_622,
		in_a => or_417,
		in_b => or_568
	);

	mul_577_i : mul_577 port map (
		result => sig_621,
		in_a => or_575,
		in_b => "010110101000001"
	);

	mul_579_i : mul_579 port map (
		result => sig_620,
		in_a => or_541,
		in_b => "0101001110011111"
	);

	sub_580_i : sub_580 port map (
		result => sig_619,
		in_a => sig_706,
		in_b => sig_620
	);

	sub_585_i : sub_585 port map (
		result => sig_618,
		in_a => or_581,
		in_b => or_583
	);

	sub_586_i : sub_586 port map (
		result => sig_617,
		in_a => sig_1115,
		in_b => sig_1161
	);

	mul_589_i : mul_589 port map (
		result => sig_616,
		in_a => or_587,
		in_b => "0101001110011111"
	);

	mul_592_i : mul_592 port map (
		result => sig_615,
		in_a => or_590,
		in_b => "010001010100011"
	);

	sub_593_i : sub_593 port map (
		result => sig_614,
		in_a => sig_616,
		in_b => sig_615
	);

	mul_594_i : mul_594 port map (
		result => sig_613,
		in_a => or_587,
		in_b => "010001010100011"
	);

	mul_595_i : mul_595 port map (
		result => sig_612,
		in_a => or_590,
		in_b => "0101001110011111"
	);

	sub_596_i : sub_596 port map (
		result => sig_611,
		in_a => sig_613,
		in_b => sig_612
	);

	sub_599_i : sub_599 port map (
		result => sig_610,
		in_a => or_423,
		in_b => or_597
	);

	add_600_i : add_600 port map (
		result => sig_609,
		in_a => or_423,
		in_b => or_597
	);

	add_601_i : add_601 port map (
		result => sig_608,
		in_a => idct_2d_yc_reg2(31 downto 5),
		in_b => "000000000000000000000000001"
	);

	add_602_i : add_602 port map (
		result => sig_607,
		in_a => sig_1160,
		in_b => "00000000000000000001"
	);

	mul_605_i : mul_605 port map (
		result => sig_606,
		in_a => or_603,
		in_b => "010110101000001"
	);

	-- Behaviour of component 'mux_66' model 'mux'
	mux_66 <=
		(repeat(3, sig_1003) and psc_loop_reg_13(2 downto 0)) or
		(repeat(3, sig_947) and "110") or
		(repeat(3, sig_945) and "101") or
		(repeat(3, sig_943) and "100") or
		(repeat(3, sig_941) and "011") or
		(repeat(3, sig_949) and "111") or
		(repeat(3, sig_939) and "010") or
		(repeat(3, sig_937) and "001");

	-- Behaviour of component 'mux_30' model 'mux'
	mux_30 <=
		(sig_873 and cp_en);

	-- Behaviour of component 'mux_32' model 'mux'
	mux_32 <=
		(sig_1002 and sig_702) or
		(sig_872 and sig_690);

	-- Behaviour of component 'mux_33' model 'mux'
	mux_33 <=
		(sig_1039 and cp_din(0)) or
		(sig_954 and '1');

	-- Behaviour of component 'mux_34' model 'mux'
	mux_34 <=
		(sig_1038 and cp_rest) or
		(sig_953 and '1');

	-- Behaviour of component 'mux_58' model 'mux'
	mux_58 <=
		(repeat(3, sig_1003) and psc_loop_reg_13(2 downto 0)) or
		(repeat(3, sig_966) and "110") or
		(repeat(3, sig_957) and "101") or
		(repeat(3, sig_956) and "100") or
		(repeat(3, sig_990) and "011") or
		(repeat(3, sig_996) and "111") or
		(repeat(3, sig_986) and "010") or
		(repeat(3, sig_984) and "001");

	-- Behaviour of component 'mux_59' model 'mux'
	mux_59 <=
		(repeat(3, sig_1003) and psc_loop_reg_13(2 downto 0)) or
		(repeat(3, sig_833) and augh_main_k(5 downto 3));

	-- Behaviour of component 'mux_60' model 'mux'
	mux_60 <=
		(sig_1004 and and_161) or
		(sig_995 and '1');

	-- Behaviour of component 'mux_61' model 'mux'
	mux_61 <=
		(repeat(8, sig_1003) and cp_din(23 downto 16)) or
		(repeat(8, sig_977) and mux_156);

	-- Behaviour of component 'mux_62' model 'mux'
	mux_62 <=
		(repeat(3, sig_1003) and psc_loop_reg_13(2 downto 0)) or
		(repeat(3, sig_974) and "110") or
		(repeat(3, sig_1018) and "101") or
		(repeat(3, sig_1016) and "100") or
		(repeat(3, sig_1014) and "011") or
		(repeat(3, sig_976) and "111") or
		(repeat(3, sig_1012) and "010") or
		(repeat(3, sig_1010) and "001");

	-- Behaviour of component 'mux_63' model 'mux'
	mux_63 <=
		(repeat(3, sig_1003) and psc_loop_reg_13(2 downto 0)) or
		(repeat(3, sig_833) and augh_main_k(5 downto 3));

	-- Behaviour of component 'mux_64' model 'mux'
	mux_64 <=
		(sig_1004 and and_161) or
		(sig_975 and '1');

	-- Behaviour of component 'mux_65' model 'mux'
	mux_65 <=
		(repeat(8, sig_1003) and cp_din(31 downto 24)) or
		(repeat(8, sig_950) and mux_156);

	-- Behaviour of component 'mux_35' model 'mux'
	mux_35 <=
		(sig_954 and '1') or
		(sig_874 and augh_main_k(0));

	-- Behaviour of component 'mux_36' model 'mux'
	mux_36 <=
		(sig_953 and '1') or
		(sig_873 and cp_en);

	-- Behaviour of component 'mux_37' model 'mux'
	mux_37 <=
		(repeat(16, sig_1052) and sig_1128) or
		(repeat(16, sig_874) and sig_691);

	-- Behaviour of component 'mux_38' model 'mux'
	mux_38 <=
		(sig_1051 and cp_en) or
		(sig_1049 and '1');

	-- Behaviour of component 'mux_39' model 'mux'
	mux_39 <=
		(repeat(32, sig_1005) and cp_din(31 downto 0)) or
		(repeat(32, sig_884) and sig_704) or
		(repeat(32, sig_880) and sig_1124) or
		(repeat(32, sig_862) and sig_1103) or
		(repeat(32, sig_859) and sig_1102) or
		(repeat(32, sig_887) and (sig_1084(28 downto 0) & "000")) or
		(repeat(32, sig_831) and read32_ret0_10) or
		(repeat(32, sig_733) and (sig_1085(28 downto 0) & "000"));

	-- Behaviour of component 'mux_45' model 'mux'
	mux_45 <=
		(repeat(5, sig_857) and "01101") or
		(repeat(5, sig_754) and "11011") or
		(repeat(5, sig_742) and "10111") or
		(repeat(5, sig_737) and "10001") or
		(repeat(5, sig_770) and "11111");

	-- Behaviour of component 'mux_46' model 'mux'
	mux_46 <=
		(repeat(5, sig_867) and "01010") or
		(repeat(5, sig_852) and "01111") or
		(repeat(5, sig_843) and "11110") or
		(repeat(5, sig_769) and "11010") or
		(repeat(5, sig_761) and "11111") or
		(repeat(5, sig_857) and "01011") or
		(repeat(5, sig_755) and "10110") or
		(repeat(5, sig_741) and "10011");

	-- Behaviour of component 'mux_47' model 'mux'
	mux_47 <=
		(repeat(5, sig_867) and "01011") or
		(repeat(5, sig_852) and "01110") or
		(repeat(5, sig_843) and "11111") or
		(repeat(5, sig_788) and "00001") or
		(repeat(5, sig_770) and "11011") or
		(repeat(5, sig_730) and "00111") or
		(repeat(5, sig_857) and "01001") or
		(repeat(5, sig_764) and "00110") or
		(repeat(5, sig_742) and "10010") or
		(repeat(5, sig_735) and "10000") or
		(repeat(5, sig_762) and "00010") or
		(repeat(5, sig_761) and "11101") or
		(repeat(5, sig_755) and "10111") or
		(repeat(5, sig_752) and "11001");

	-- Behaviour of component 'mux_48' model 'mux'
	mux_48 <=
		(repeat(5, sig_1005) and psc_loop_reg_13(4 downto 0)) or
		(repeat(5, sig_865) and "01101") or
		(repeat(5, sig_844) and "11101") or
		(repeat(5, sig_823) and "11000") or
		(repeat(5, sig_818) and "10100") or
		(repeat(5, sig_810) and "00100") or
		(repeat(5, sig_804) and "10001") or
		(repeat(5, sig_797) and "10101") or
		(repeat(5, sig_785) and "11001") or
		(repeat(5, sig_758) and "10000") or
		(repeat(5, sig_731) and "00101") or
		(repeat(5, sig_1017) and "11011") or
		(repeat(5, sig_1015) and "10111") or
		(repeat(5, sig_1013) and "10011") or
		(repeat(5, sig_1011) and "01111") or
		(repeat(5, sig_918) and "01110") or
		(repeat(5, sig_916) and "01010") or
		(repeat(5, sig_915) and "00110") or
		(repeat(5, sig_973) and "11111") or
		(repeat(5, sig_913) and "00010") or
		(repeat(5, sig_891) and "01001") or
		(repeat(5, sig_871) and "01100") or
		(repeat(5, sig_1009) and "01011") or
		(repeat(5, sig_922) and "10110") or
		(repeat(5, sig_920) and "10010") or
		(repeat(5, sig_1058) and "00111") or
		(repeat(5, sig_1056) and "00011") or
		(repeat(5, sig_926) and "11110") or
		(repeat(5, sig_924) and "11010");

	-- Behaviour of component 'mux_49' model 'mux'
	mux_49 <=
		(repeat(5, sig_866) and "01000") or
		(repeat(5, sig_842) and "11100") or
		(repeat(5, sig_787) and "00011") or
		(repeat(5, sig_770) and "11000") or
		(repeat(5, sig_755) and "10100") or
		(repeat(5, sig_850) and "01101") or
		(repeat(5, sig_752) and "11011") or
		(repeat(5, sig_742) and "10001") or
		(repeat(5, sig_735) and "10010");

	-- Behaviour of component 'mux_40' model 'mux'
	mux_40 <=
		(repeat(5, sig_1005) and psc_loop_reg_13(4 downto 0)) or
		(repeat(5, sig_794) and "11000") or
		(repeat(5, sig_782) and "10100") or
		(repeat(5, sig_781) and "11100") or
		(repeat(5, sig_780) and "11110") or
		(repeat(5, sig_776) and "11001") or
		(repeat(5, sig_773) and "01001") or
		(repeat(5, sig_771) and "10011") or
		(repeat(5, sig_763) and "10001") or
		(repeat(5, sig_760) and "11010") or
		(repeat(5, sig_759) and "10101") or
		(repeat(5, sig_751) and "10110") or
		(repeat(5, sig_750) and "00111") or
		(repeat(5, sig_748) and "01011") or
		(repeat(5, sig_744) and "01110") or
		(repeat(5, sig_736) and "01100") or
		(repeat(5, sig_883) and "00101") or
		(repeat(5, sig_879) and "00010") or
		(repeat(5, sig_875) and "00110") or
		(repeat(5, sig_863) and "01010") or
		(repeat(5, sig_828) and "10000") or
		(repeat(5, sig_827) and "01111") or
		(repeat(5, sig_886) and "01101") or
		(repeat(5, sig_824) and "11011") or
		(repeat(5, sig_807) and "00001") or
		(repeat(5, sig_803) and "11111") or
		(repeat(5, sig_861) and "00100") or
		(repeat(5, sig_845) and "10111") or
		(repeat(5, sig_831) and augh_main_k(5 downto 1)) or
		(repeat(5, sig_860) and "10010") or
		(repeat(5, sig_858) and "00011") or
		(repeat(5, sig_854) and "01000") or
		(repeat(5, sig_849) and "11101");

	-- Behaviour of component 'mux_41' model 'mux'
	mux_41 <=
		(repeat(5, sig_857) and "01110") or
		(repeat(5, sig_754) and "11010") or
		(repeat(5, sig_742) and "10110") or
		(repeat(5, sig_737) and "10010") or
		(repeat(5, sig_770) and "11110");

	-- Behaviour of component 'mux_42' model 'mux'
	mux_42 <=
		(sig_1053 and and_161) or
		(sig_830 and sig_672) or
		(sig_885 and '1');

	-- Behaviour of component 'mux_43' model 'mux'
	mux_43 <=
		(repeat(5, sig_857) and "01111") or
		(repeat(5, sig_826) and "00001") or
		(repeat(5, sig_808) and "01011") or
		(repeat(5, sig_768) and "11100") or
		(repeat(5, sig_754) and "11000") or
		(repeat(5, sig_836) and "00111") or
		(repeat(5, sig_749) and "00101") or
		(repeat(5, sig_728) and "00011") or
		(repeat(5, sig_747) and "00110") or
		(repeat(5, sig_742) and "10100") or
		(repeat(5, sig_740) and "00010") or
		(repeat(5, sig_737) and "10011");

	-- Behaviour of component 'mux_44' model 'mux'
	mux_44 <=
		(repeat(5, sig_1005) and psc_loop_reg_13(4 downto 0)) or
		(repeat(5, sig_902) and "10101") or
		(repeat(5, sig_900) and "10001") or
		(repeat(5, sig_898) and "01101") or
		(repeat(5, sig_896) and "01001") or
		(repeat(5, sig_895) and "00101") or
		(repeat(5, sig_893) and "00001") or
		(repeat(5, sig_856) and "01100") or
		(repeat(5, sig_838) and "10100") or
		(repeat(5, sig_837) and "00100") or
		(repeat(5, sig_829) and "10000") or
		(repeat(5, sig_821) and "01000") or
		(repeat(5, sig_800) and "11000") or
		(repeat(5, sig_958) and "11011") or
		(repeat(5, sig_991) and "10111") or
		(repeat(5, sig_987) and "10011") or
		(repeat(5, sig_985) and "01111") or
		(repeat(5, sig_938) and "01110") or
		(repeat(5, sig_936) and "01010") or
		(repeat(5, sig_935) and "00110") or
		(repeat(5, sig_961) and "11111") or
		(repeat(5, sig_933) and "00010") or
		(repeat(5, sig_906) and "11101") or
		(repeat(5, sig_904) and "11001") or
		(repeat(5, sig_983) and "01011") or
		(repeat(5, sig_942) and "10110") or
		(repeat(5, sig_940) and "10010") or
		(repeat(5, sig_982) and "00111") or
		(repeat(5, sig_980) and "00011") or
		(repeat(5, sig_946) and "11110") or
		(repeat(5, sig_944) and "11010");

	-- Behaviour of component 'mux_50' model 'mux'
	mux_50 <=
		(repeat(32, sig_1005) and cp_din(63 downto 32)) or
		(repeat(32, sig_882) and sig_1111) or
		(repeat(32, sig_877) and sig_1079) or
		(repeat(32, sig_869) and sig_1149) or
		(repeat(32, sig_847) and sig_1150) or
		(repeat(32, sig_890) and (sig_1093(28 downto 0) & "000")) or
		(repeat(32, sig_831) and read32_ret0_10) or
		(repeat(32, sig_777) and (sig_1094(28 downto 0) & "000")) or
		(repeat(32, sig_766) and (sig_1092(28 downto 0) & "000"));

	-- Behaviour of component 'mux_51' model 'mux'
	mux_51 <=
		(repeat(5, sig_1005) and psc_loop_reg_13(4 downto 0)) or
		(repeat(5, sig_796) and "10101") or
		(repeat(5, sig_795) and "10110") or
		(repeat(5, sig_793) and "11111") or
		(repeat(5, sig_790) and "11101") or
		(repeat(5, sig_779) and "11010") or
		(repeat(5, sig_778) and "01011") or
		(repeat(5, sig_775) and "11001") or
		(repeat(5, sig_767) and "11100") or
		(repeat(5, sig_765) and "01000") or
		(repeat(5, sig_756) and "10000") or
		(repeat(5, sig_753) and "10011") or
		(repeat(5, sig_746) and "10010") or
		(repeat(5, sig_745) and "01010") or
		(repeat(5, sig_729) and "11110") or
		(repeat(5, sig_881) and "00110") or
		(repeat(5, sig_878) and "01111") or
		(repeat(5, sig_876) and "00101") or
		(repeat(5, sig_870) and "01100") or
		(repeat(5, sig_817) and "10100") or
		(repeat(5, sig_813) and "00010") or
		(repeat(5, sig_812) and "00001") or
		(repeat(5, sig_889) and "01001") or
		(repeat(5, sig_809) and "00100") or
		(repeat(5, sig_799) and "10001") or
		(repeat(5, sig_798) and "11011") or
		(repeat(5, sig_868) and "00011") or
		(repeat(5, sig_831) and augh_main_k(5 downto 1)) or
		(repeat(5, sig_819) and "10111") or
		(repeat(5, sig_864) and "01101") or
		(repeat(5, sig_855) and "01110") or
		(repeat(5, sig_853) and "00111") or
		(repeat(5, sig_846) and "11000");

	-- Behaviour of component 'mux_52' model 'mux'
	mux_52 <=
		(sig_1053 and and_161) or
		(sig_830 and sig_1064) or
		(sig_888 and '1');

	-- Behaviour of component 'mux_53' model 'mux'
	mux_53 <=
		(repeat(8, sig_1008) and mux_156) or
		(repeat(8, sig_1003) and cp_din(7 downto 0));

	-- Behaviour of component 'mux_54' model 'mux'
	mux_54 <=
		(repeat(3, sig_1007) and "111") or
		(repeat(3, sig_1045) and "101") or
		(repeat(3, sig_1043) and "100") or
		(repeat(3, sig_1041) and "011") or
		(repeat(3, sig_1037) and "010") or
		(repeat(3, sig_1048) and "110") or
		(repeat(3, sig_1035) and "001") or
		(repeat(3, sig_1003) and psc_loop_reg_13(2 downto 0));

	-- Behaviour of component 'mux_55' model 'mux'
	mux_55 <=
		(repeat(3, sig_1003) and psc_loop_reg_13(2 downto 0)) or
		(repeat(3, sig_833) and augh_main_k(5 downto 3));

	-- Behaviour of component 'mux_56' model 'mux'
	mux_56 <=
		(sig_1006 and '1') or
		(sig_1004 and and_161);

	-- Behaviour of component 'mux_57' model 'mux'
	mux_57 <=
		(repeat(8, sig_1003) and cp_din(15 downto 8)) or
		(repeat(8, sig_997) and mux_156);

	-- Behaviour of component 'mux_88' model 'mux'
	mux_88 <=
		(sig_1004 and and_161) or
		(sig_839 and '1');

	-- Behaviour of component 'mux_67' model 'mux'
	mux_67 <=
		(repeat(3, sig_1003) and psc_loop_reg_13(2 downto 0)) or
		(repeat(3, sig_833) and augh_main_k(5 downto 3));

	-- Behaviour of component 'mux_68' model 'mux'
	mux_68 <=
		(sig_1004 and and_161) or
		(sig_948 and '1');

	-- Behaviour of component 'mux_69' model 'mux'
	mux_69 <=
		(repeat(32, sig_1026) and sig_1102) or
		(repeat(32, sig_974) and or_244) or
		(repeat(32, sig_947) and or_266) or
		(repeat(32, sig_927) and or_285) or
		(repeat(32, sig_907) and or_304) or
		(repeat(32, sig_966) and or_221) or
		(repeat(32, sig_802) and or_378) or
		(repeat(32, sig_727) and or_371) or
		(repeat(32, sig_723) and or_470);

	-- Behaviour of component 'mux_71' model 'mux'
	mux_71 <=
		(repeat(32, sig_1026) and sig_1149) or
		(repeat(32, sig_1018) and or_246) or
		(repeat(32, sig_945) and or_268) or
		(repeat(32, sig_925) and or_287) or
		(repeat(32, sig_905) and or_306) or
		(repeat(32, sig_957) and or_224) or
		(repeat(32, sig_801) and or_433) or
		(repeat(32, sig_789) and or_438) or
		(repeat(32, sig_786) and or_450);

	-- Behaviour of component 'mux_73' model 'mux'
	mux_73 <=
		(repeat(32, sig_1026) and sig_1124) or
		(repeat(32, sig_1016) and or_248) or
		(repeat(32, sig_943) and or_270) or
		(repeat(32, sig_923) and or_289) or
		(repeat(32, sig_903) and or_308) or
		(repeat(32, sig_956) and or_228) or
		(repeat(32, sig_840) and or_384) or
		(repeat(32, sig_792) and or_435) or
		(repeat(32, sig_774) and or_452);

	-- Behaviour of component 'mux_75' model 'mux'
	mux_75 <=
		(repeat(32, sig_1026) and sig_1111) or
		(repeat(32, sig_1014) and or_250) or
		(repeat(32, sig_941) and or_272) or
		(repeat(32, sig_921) and or_291) or
		(repeat(32, sig_901) and or_310) or
		(repeat(32, sig_990) and or_231) or
		(repeat(32, sig_825) and or_417) or
		(repeat(32, sig_805) and or_431) or
		(repeat(32, sig_757) and or_497);

	-- Behaviour of component 'mux_77' model 'mux'
	mux_77 <=
		(repeat(32, sig_1026) and sig_704) or
		(repeat(32, sig_1012) and or_252) or
		(repeat(32, sig_939) and or_274) or
		(repeat(32, sig_919) and or_293) or
		(repeat(32, sig_899) and or_312) or
		(repeat(32, sig_986) and or_233) or
		(repeat(32, sig_806) and or_363) or
		(repeat(32, sig_783) and or_346) or
		(repeat(32, sig_743) and or_358);

	-- Behaviour of component 'mux_79' model 'mux'
	mux_79 <=
		(repeat(32, sig_1026) and sig_1079) or
		(repeat(32, sig_1010) and or_256) or
		(repeat(32, sig_937) and or_278) or
		(repeat(32, sig_917) and or_297) or
		(repeat(32, sig_897) and or_316) or
		(repeat(32, sig_984) and or_237) or
		(repeat(32, sig_822) and or_421) or
		(repeat(32, sig_738) and or_333) or
		(repeat(32, sig_726) and or_326);

	-- Behaviour of component 'mux_81' model 'mux'
	mux_81 <=
		(repeat(32, sig_1026) and sig_1103) or
		(repeat(32, sig_1057) and or_258) or
		(repeat(32, sig_934) and or_280) or
		(repeat(32, sig_914) and or_299) or
		(repeat(32, sig_894) and or_318) or
		(repeat(32, sig_981) and or_239) or
		(repeat(32, sig_784) and or_386) or
		(repeat(32, sig_734) and or_479) or
		(repeat(32, sig_724) and or_587);

	-- Behaviour of component 'mux_83' model 'mux'
	mux_83 <=
		(repeat(32, sig_1026) and sig_1150) or
		(repeat(32, sig_1055) and or_260) or
		(repeat(32, sig_932) and or_282) or
		(repeat(32, sig_912) and or_301) or
		(repeat(32, sig_892) and or_320) or
		(repeat(32, sig_979) and or_241) or
		(repeat(32, sig_820) and or_423) or
		(repeat(32, sig_811) and or_425) or
		(repeat(32, sig_722) and or_440);

	-- Behaviour of component 'mux_85' model 'mux'
	mux_85 <=
		(repeat(8, sig_1003) and cp_din(63 downto 56)) or
		(repeat(8, sig_841) and mux_156);

	-- Behaviour of component 'mux_86' model 'mux'
	mux_86 <=
		(repeat(3, sig_1003) and psc_loop_reg_13(2 downto 0)) or
		(repeat(3, sig_825) and "011") or
		(repeat(3, sig_822) and "001") or
		(repeat(3, sig_801) and "101") or
		(repeat(3, sig_840) and "100") or
		(repeat(3, sig_783) and "010") or
		(repeat(3, sig_725) and "111") or
		(repeat(3, sig_723) and "110");

	-- Behaviour of component 'mux_87' model 'mux'
	mux_87 <=
		(repeat(3, sig_1003) and psc_loop_reg_13(2 downto 0)) or
		(repeat(3, sig_833) and augh_main_k(5 downto 3));

	-- Behaviour of component 'mux_28' model 'mux'
	mux_28 <=
		(sig_873 and cp_en);

	-- Behaviour of component 'mux_109' model 'mux'
	mux_109 <=
		(repeat(32, sig_972) and sig_1119) or
		(repeat(32, sig_836) and sig_677) or
		(repeat(32, sig_808) and sig_669) or
		(repeat(32, sig_770) and sig_660) or
		(repeat(32, sig_754) and sig_645) or
		(repeat(32, sig_857) and sig_683) or
		(repeat(32, sig_742) and sig_634) or
		(repeat(32, sig_737) and sig_624) or
		(repeat(32, sig_728) and sig_609);

	-- Behaviour of component 'mux_154' model 'mux'
	mux_154 <=
		(sig_952 and sig_699);

	-- Behaviour of component 'mux_156' model 'mux'
	mux_156 <=
		(repeat(8, sig_1130) and mux_158);

	-- Behaviour of component 'mux_89' model 'mux'
	mux_89 <=
		(repeat(8, sig_1003) and cp_din(55 downto 48)) or
		(repeat(8, sig_816) and mux_156);

	-- Behaviour of component 'mux_90' model 'mux'
	mux_90 <=
		(repeat(3, sig_1003) and psc_loop_reg_13(2 downto 0)) or
		(repeat(3, sig_806) and "010") or
		(repeat(3, sig_805) and "011") or
		(repeat(3, sig_792) and "100") or
		(repeat(3, sig_786) and "101") or
		(repeat(3, sig_815) and "111") or
		(repeat(3, sig_727) and "110") or
		(repeat(3, sig_726) and "001");

	-- Behaviour of component 'mux_134' model 'mux'
	mux_134 <=
		(sig_873 and cp_en) or
		(sig_832 and '1');

	-- Behaviour of component 'mux_91' model 'mux'
	mux_91 <=
		(repeat(3, sig_1003) and psc_loop_reg_13(2 downto 0)) or
		(repeat(3, sig_833) and augh_main_k(5 downto 3));

	-- Behaviour of component 'mux_92' model 'mux'
	mux_92 <=
		(sig_1004 and and_161) or
		(sig_814 and '1');

	-- Behaviour of component 'mux_158' model 'mux'
	mux_158 <=
		(repeat(8, sig_1157) and "11111111") or
		(repeat(8, sig_1106) and idct_2d_r(7 downto 0));

	-- Behaviour of component 'mux_111' model 'mux'
	mux_111 <=
		(repeat(32, sig_960) and (sig_1132 & '0')) or
		(repeat(32, sig_770) and (sig_659 & '0')) or
		(repeat(32, sig_754) and (sig_644 & '0')) or
		(repeat(32, sig_747) and (sig_640 & '0')) or
		(repeat(32, sig_742) and (sig_632 & '0')) or
		(repeat(32, sig_857) and (sig_1076 & '0')) or
		(repeat(32, sig_740) and (sig_631 & '0')) or
		(repeat(32, sig_737) and (sig_621 & '0')) or
		(repeat(32, sig_721) and (sig_606 & '0'));

	-- Behaviour of component 'mux_113' model 'mux'
	mux_113 <=
		(repeat(32, sig_989) and (sig_1112 & '0')) or
		(repeat(32, sig_843) and (sig_678 & '0')) or
		(repeat(32, sig_826) and (sig_671 & '0')) or
		(repeat(32, sig_772) and (sig_712 & '0')) or
		(repeat(32, sig_770) and (sig_661 & '0')) or
		(repeat(32, sig_857) and (sig_682 & '0')) or
		(repeat(32, sig_754) and (sig_646 & '0')) or
		(repeat(32, sig_749) and (sig_641 & '0')) or
		(repeat(32, sig_742) and (sig_635 & '0')) or
		(repeat(32, sig_737) and (sig_623 & '0'));

	-- Behaviour of component 'mux_115' model 'mux'
	mux_115 <=
		(repeat(32, sig_972) and sig_1142) or
		(repeat(32, sig_836) and sig_676) or
		(repeat(32, sig_808) and sig_1063) or
		(repeat(32, sig_770) and sig_658) or
		(repeat(32, sig_754) and sig_709) or
		(repeat(32, sig_857) and sig_681) or
		(repeat(32, sig_742) and sig_633) or
		(repeat(32, sig_737) and sig_622) or
		(repeat(32, sig_728) and sig_610);

	-- Behaviour of component 'mux_117' model 'mux'
	mux_117 <=
		(repeat(32, sig_965) and sig_1136) or
		(repeat(32, sig_843) and sig_1071) or
		(repeat(32, sig_787) and sig_713) or
		(repeat(32, sig_770) and sig_710) or
		(repeat(32, sig_754) and sig_648) or
		(repeat(32, sig_857) and sig_684) or
		(repeat(32, sig_742) and sig_636) or
		(repeat(32, sig_737) and sig_625) or
		(repeat(32, sig_730) and sig_614);

	-- Behaviour of component 'mux_119' model 'mux'
	mux_119 <=
		(repeat(32, sig_963) and sig_1133) or
		(repeat(32, sig_851) and sig_1073) or
		(repeat(32, sig_787) and sig_663) or
		(repeat(32, sig_761) and sig_1061) or
		(repeat(32, sig_752) and sig_1059) or
		(repeat(32, sig_867) and sig_1078) or
		(repeat(32, sig_739) and sig_629) or
		(repeat(32, sig_735) and sig_619) or
		(repeat(32, sig_730) and sig_611);

	-- Behaviour of component 'mux_121' model 'mux'
	mux_121 <=
		(repeat(32, sig_993) and sig_1131) or
		(repeat(32, sig_851) and sig_716) or
		(repeat(32, sig_764) and sig_656) or
		(repeat(32, sig_762) and sig_654) or
		(repeat(32, sig_761) and sig_653) or
		(repeat(32, sig_867) and sig_687) or
		(repeat(32, sig_752) and sig_1060) or
		(repeat(32, sig_739) and sig_628) or
		(repeat(32, sig_735) and sig_618);

	-- Behaviour of component 'mux_123' model 'mux'
	mux_123 <=
		(repeat(32, sig_955) and sig_1109) or
		(repeat(32, sig_843) and sig_679) or
		(repeat(32, sig_770) and sig_1062) or
		(repeat(32, sig_764) and sig_657) or
		(repeat(32, sig_762) and sig_655) or
		(repeat(32, sig_857) and sig_686) or
		(repeat(32, sig_754) and sig_647) or
		(repeat(32, sig_742) and sig_639) or
		(repeat(32, sig_737) and sig_627);

	-- Behaviour of component 'or_224' model 'or'
	or_224 <=
		and_225;

	-- Behaviour of component 'and_225' model 'and'
	and_225 <=
		sig_1084;

	-- Behaviour of component 'or_231' model 'or'
	or_231 <=
		and_232;

	-- Behaviour of component 'and_232' model 'and'
	and_232 <=
		sig_1084;

	-- Behaviour of component 'or_250' model 'or'
	or_250 <=
		and_251;

	-- Behaviour of component 'and_251' model 'and'
	and_251 <=
		sig_1093;

	-- Behaviour of component 'or_260' model 'or'
	or_260 <=
		and_261;

	-- Behaviour of component 'and_261' model 'and'
	and_261 <=
		sig_1093;

	-- Behaviour of component 'or_282' model 'or'
	or_282 <=
		and_283;

	-- Behaviour of component 'and_283' model 'and'
	and_283 <=
		sig_1084;

	-- Behaviour of component 'or_285' model 'or'
	or_285 <=
		and_286;

	-- Behaviour of component 'and_286' model 'and'
	and_286 <=
		sig_1093;

	-- Behaviour of component 'or_289' model 'or'
	or_289 <=
		and_290;

	-- Behaviour of component 'and_290' model 'and'
	and_290 <=
		sig_1093;

	-- Behaviour of component 'or_291' model 'or'
	or_291 <=
		and_292;

	-- Behaviour of component 'and_292' model 'and'
	and_292 <=
		sig_1093;

	-- Behaviour of component 'or_297' model 'or'
	or_297 <=
		and_298;

	-- Behaviour of component 'and_298' model 'and'
	and_298 <=
		sig_1093;

	-- Behaviour of component 'or_299' model 'or'
	or_299 <=
		and_300;

	-- Behaviour of component 'and_300' model 'and'
	and_300 <=
		sig_1093;

	-- Behaviour of component 'or_320' model 'or'
	or_320 <=
		and_321;

	-- Behaviour of component 'and_321' model 'and'
	and_321 <=
		sig_1084;

	-- Behaviour of component 'or_326' model 'or'
	or_326 <=
		and_327;

	-- Behaviour of component 'and_327' model 'and'
	and_327 <=
		sig_1093;

	-- Behaviour of component 'or_333' model 'or'
	or_333 <=
		and_334;

	-- Behaviour of component 'and_334' model 'and'
	and_334 <=
		sig_1092;

	-- Behaviour of component 'or_363' model 'or'
	or_363 <=
		and_364;

	-- Behaviour of component 'and_364' model 'and'
	and_364 <=
		sig_1092;

	-- Behaviour of component 'and_403' model 'and'
	and_403 <=
		sig_1158 and
		repeat(8, sig_1068);

	-- Behaviour of component 'and_405' model 'and'
	and_405 <=
		sig_1159 and
		repeat(8, sig_1067);

	-- Behaviour of component 'and_407' model 'and'
	and_407 <=
		sig_1154 and
		repeat(8, sig_1066);

	-- Behaviour of component 'and_409' model 'and'
	and_409 <=
		sig_1153 and
		repeat(8, sig_1065);

	-- Behaviour of component 'and_415' model 'and'
	and_415 <=
		sig_1085(30 downto 0);

	-- Behaviour of component 'or_464' model 'or'
	or_464 <=
		and_465;

	-- Behaviour of component 'and_465' model 'and'
	and_465 <=
		sig_1095;

	-- Behaviour of component 'or_470' model 'or'
	or_470 <=
		and_471;

	-- Behaviour of component 'and_471' model 'and'
	and_471 <=
		sig_1085;

	-- Behaviour of component 'or_472' model 'or'
	or_472 <=
		and_473;

	-- Behaviour of component 'and_473' model 'and'
	and_473 <=
		sig_1083;

	-- Behaviour of component 'or_500' model 'or'
	or_500 <=
		and_501;

	-- Behaviour of component 'and_501' model 'and'
	and_501 <=
		sig_1094;

	-- Behaviour of component 'or_504' model 'or'
	or_504 <=
		and_505;

	-- Behaviour of component 'and_505' model 'and'
	and_505 <=
		sig_1092;

	-- Behaviour of component 'or_506' model 'or'
	or_506 <=
		and_507;

	-- Behaviour of component 'and_507' model 'and'
	and_507 <=
		sig_1095;

	-- Behaviour of component 'or_514' model 'or'
	or_514 <=
		and_515;

	-- Behaviour of component 'and_515' model 'and'
	and_515 <=
		sig_1083;

	-- Behaviour of component 'or_522' model 'or'
	or_522 <=
		and_523;

	-- Behaviour of component 'and_523' model 'and'
	and_523 <=
		sig_1094;

	-- Behaviour of component 'mux_129' model 'mux'
	mux_129 <=
		(repeat(32, sig_1021) and sig_1114) or
		(repeat(32, sig_1054) and sig_1100) or
		(repeat(32, sig_931) and sig_1087) or
		(repeat(32, sig_911) and sig_694) or
		(repeat(32, sig_848) and sig_1072) or
		(repeat(32, sig_978) and sig_1101) or
		(repeat(32, sig_791) and sig_668) or
		(repeat(32, sig_732) and sig_617);

	-- Behaviour of component 'mux_133' model 'mux'
	mux_133 <=
		(repeat(8, sig_874) and cp_din(39 downto 32)) or
		(repeat(8, sig_833) and or_394);

	-- Behaviour of component 'mux_135' model 'mux'
	mux_135 <=
		(repeat(32, sig_1047) and (repeat(5, sig_1082(19)) & sig_1082 & sig_1155(7 downto 1))) or
		(repeat(32, sig_1042) and (repeat(5, sig_1127(19)) & sig_1127 & sig_719(7 downto 1))) or
		(repeat(32, sig_1040) and (repeat(5, sig_1152(19)) & sig_1152 & sig_1118(7 downto 1))) or
		(repeat(32, sig_1036) and (repeat(5, sig_1126(19)) & sig_1126 & sig_1104(7 downto 1))) or
		(repeat(32, sig_1035) and (repeat(5, sig_1151(19)) & sig_1151 & sig_718(7 downto 1))) or
		(repeat(32, sig_822) and (repeat(5, sig_714(19)) & sig_714 & sig_670(7 downto 1))) or
		(repeat(32, sig_726) and (repeat(5, sig_607(19)) & sig_607 & sig_608(7 downto 1))) or
		(repeat(32, sig_1044) and (repeat(5, sig_1081(19)) & sig_1081 & sig_720(7 downto 1))) or
		(repeat(32, sig_1034) and (repeat(5, sig_1080(19)) & sig_1080 & sig_1117(7 downto 1))) or
		(repeat(32, sig_917) and (repeat(5, sig_695(19)) & sig_695 & sig_696(7 downto 1))) or
		(repeat(32, sig_897) and (repeat(5, sig_693(19)) & sig_693 & sig_1107(7 downto 1))) or
		(repeat(32, sig_1033) and (repeat(5, sig_1125(19)) & sig_1125 & sig_1116(7 downto 1))) or
		(repeat(32, sig_984) and (repeat(5, sig_700(19)) & sig_700 & sig_1108(7 downto 1))) or
		(repeat(32, sig_1010) and (repeat(5, sig_717(19)) & sig_717 & sig_1089(7 downto 1))) or
		(repeat(32, sig_937) and (repeat(5, sig_697(19)) & sig_697 & sig_1088(7 downto 1)));

	-- Behaviour of component 'mux_137' model 'mux'
	mux_137 <=
		(repeat(8, sig_1003) and cp_din(39 downto 32)) or
		(repeat(8, sig_930) and mux_156);

	-- Behaviour of component 'mux_138' model 'mux'
	mux_138 <=
		(repeat(3, sig_1003) and psc_loop_reg_13(2 downto 0)) or
		(repeat(3, sig_927) and "110") or
		(repeat(3, sig_925) and "101") or
		(repeat(3, sig_923) and "100") or
		(repeat(3, sig_921) and "011") or
		(repeat(3, sig_929) and "111") or
		(repeat(3, sig_919) and "010") or
		(repeat(3, sig_917) and "001");

	-- Behaviour of component 'mux_139' model 'mux'
	mux_139 <=
		(repeat(3, sig_1003) and psc_loop_reg_13(2 downto 0)) or
		(repeat(3, sig_833) and augh_main_k(5 downto 3));

	-- Behaviour of component 'mux_140' model 'mux'
	mux_140 <=
		(sig_1004 and and_161) or
		(sig_928 and '1');

	-- Behaviour of component 'mux_141' model 'mux'
	mux_141 <=
		(repeat(8, sig_1003) and cp_din(47 downto 40)) or
		(repeat(8, sig_910) and mux_156);

	-- Behaviour of component 'mux_142' model 'mux'
	mux_142 <=
		(repeat(3, sig_1003) and psc_loop_reg_13(2 downto 0)) or
		(repeat(3, sig_907) and "110") or
		(repeat(3, sig_905) and "101") or
		(repeat(3, sig_903) and "100") or
		(repeat(3, sig_901) and "011") or
		(repeat(3, sig_909) and "111") or
		(repeat(3, sig_899) and "010") or
		(repeat(3, sig_897) and "001");

	-- Behaviour of component 'mux_143' model 'mux'
	mux_143 <=
		(repeat(3, sig_1003) and psc_loop_reg_13(2 downto 0)) or
		(repeat(3, sig_833) and augh_main_k(5 downto 3));

	-- Behaviour of component 'mux_144' model 'mux'
	mux_144 <=
		(sig_1004 and and_161) or
		(sig_908 and '1');

	-- Behaviour of component 'mux_147' model 'mux'
	mux_147 <=
		(sig_951 and not_264);

	-- Behaviour of component 'mux_149' model 'mux'
	mux_149 <=
		(repeat(32, sig_874) and cp_din(31 downto 0)) or
		(repeat(32, sig_835) and sig_675);

	-- Behaviour of component 'mux_150' model 'mux'
	mux_150 <=
		(sig_873 and cp_en) or
		(sig_834 and '1');

	-- Behaviour of component 'mux_151' model 'mux'
	mux_151 <=
		(sig_1005 and sig_705) or
		(sig_1003 and sig_703) or
		(sig_874 and sig_692) or
		(sig_1050 and '1');

	-- Behaviour of component 'mux_152' model 'mux'
	mux_152 <=
		(repeat(64, sig_1005) and (sig_1093 & sig_1084)) or
		(repeat(64, sig_874) and (psc_stuff_reg_19 & cp_id_reg_14)) or
		(repeat(64, sig_1003) and (sig_1153 & sig_1154 & sig_1159 & sig_1158 & sig_1099 & sig_1098 & sig_1097 & sig_1096));

	-- Behaviour of component 'mux_155' model 'mux'
	mux_155 <=
		(sig_951 and sig_698);

	-- Behaviour of component 'or_221' model 'or'
	or_221 <=
		and_222;

	-- Behaviour of component 'and_222' model 'and'
	and_222 <=
		sig_1084;

	-- Behaviour of component 'or_233' model 'or'
	or_233 <=
		and_234;

	-- Behaviour of component 'and_234' model 'and'
	and_234 <=
		sig_1084;

	-- Behaviour of component 'or_237' model 'or'
	or_237 <=
		and_238;

	-- Behaviour of component 'and_238' model 'and'
	and_238 <=
		sig_1084;

	-- Behaviour of component 'or_252' model 'or'
	or_252 <=
		and_253;

	-- Behaviour of component 'and_253' model 'and'
	and_253 <=
		sig_1093;

	-- Behaviour of component 'or_256' model 'or'
	or_256 <=
		and_257;

	-- Behaviour of component 'and_257' model 'and'
	and_257 <=
		sig_1093;

	-- Behaviour of component 'or_268' model 'or'
	or_268 <=
		and_269;

	-- Behaviour of component 'and_269' model 'and'
	and_269 <=
		sig_1084;

	-- Behaviour of component 'or_270' model 'or'
	or_270 <=
		and_271;

	-- Behaviour of component 'and_271' model 'and'
	and_271 <=
		sig_1084;

	-- Behaviour of component 'or_274' model 'or'
	or_274 <=
		and_275;

	-- Behaviour of component 'and_275' model 'and'
	and_275 <=
		sig_1084;

	-- Behaviour of component 'or_278' model 'or'
	or_278 <=
		and_279;

	-- Behaviour of component 'and_279' model 'and'
	and_279 <=
		sig_1084;

	-- Behaviour of component 'or_310' model 'or'
	or_310 <=
		and_311;

	-- Behaviour of component 'and_311' model 'and'
	and_311 <=
		sig_1084;

	-- Behaviour of component 'or_316' model 'or'
	or_316 <=
		and_317;

	-- Behaviour of component 'and_317' model 'and'
	and_317 <=
		sig_1084;

	-- Behaviour of component 'or_358' model 'or'
	or_358 <=
		and_359;

	-- Behaviour of component 'and_359' model 'and'
	and_359 <=
		sig_1093;

	-- Behaviour of component 'or_366' model 'or'
	or_366 <=
		and_367;

	-- Behaviour of component 'and_367' model 'and'
	and_367 <=
		sig_1095;

	-- Behaviour of component 'or_374' model 'or'
	or_374 <=
		and_375;

	-- Behaviour of component 'and_375' model 'and'
	and_375 <=
		sig_1094;

	-- Behaviour of component 'or_417' model 'or'
	or_417 <=
		and_418;

	-- Behaviour of component 'and_418' model 'and'
	and_418 <=
		sig_1084;

	-- Behaviour of component 'or_421' model 'or'
	or_421 <=
		and_422;

	-- Behaviour of component 'and_422' model 'and'
	and_422 <=
		sig_1084;

	-- Behaviour of component 'or_435' model 'or'
	or_435 <=
		and_436;

	-- Behaviour of component 'and_436' model 'and'
	and_436 <=
		sig_1093;

	-- Behaviour of component 'or_452' model 'or'
	or_452 <=
		and_453;

	-- Behaviour of component 'and_453' model 'and'
	and_453 <=
		sig_1093;

	-- Behaviour of component 'and_494' model 'and'
	and_494 <=
		sig_1095;

	-- Behaviour of component 'and_498' model 'and'
	and_498 <=
		sig_1093;

	-- Behaviour of component 'or_509' model 'or'
	or_509 <=
		and_510;

	-- Behaviour of component 'and_510' model 'and'
	and_510 <=
		sig_1084(30 downto 0);

	-- Behaviour of component 'or_550' model 'or'
	or_550 <=
		and_551;

	-- Behaviour of component 'and_551' model 'and'
	and_551 <=
		sig_1083;

	-- Behaviour of component 'or_581' model 'or'
	or_581 <=
		and_582;

	-- Behaviour of component 'and_582' model 'and'
	and_582 <=
		sig_1094;

	-- Behaviour of component 'or_583' model 'or'
	or_583 <=
		and_584;

	-- Behaviour of component 'and_584' model 'and'
	and_584 <=
		sig_1092;

	-- Behaviour of component 'or_587' model 'or'
	or_587 <=
		and_588;

	-- Behaviour of component 'and_588' model 'and'
	and_588 <=
		sig_1093;

	-- Behaviour of component 'and_161' model 'and'
	and_161 <=
		cp_en and
		cp_rest;

	-- Behaviour of component 'or_228' model 'or'
	or_228 <=
		and_229;

	-- Behaviour of component 'and_229' model 'and'
	and_229 <=
		sig_1084;

	-- Behaviour of component 'or_239' model 'or'
	or_239 <=
		and_240;

	-- Behaviour of component 'and_240' model 'and'
	and_240 <=
		sig_1084;

	-- Behaviour of component 'or_241' model 'or'
	or_241 <=
		and_242;

	-- Behaviour of component 'and_242' model 'and'
	and_242 <=
		sig_1084;

	-- Behaviour of component 'or_244' model 'or'
	or_244 <=
		and_245;

	-- Behaviour of component 'and_245' model 'and'
	and_245 <=
		sig_1093;

	-- Behaviour of component 'or_246' model 'or'
	or_246 <=
		and_247;

	-- Behaviour of component 'and_247' model 'and'
	and_247 <=
		sig_1093;

	-- Behaviour of component 'or_248' model 'or'
	or_248 <=
		and_249;

	-- Behaviour of component 'and_249' model 'and'
	and_249 <=
		sig_1093;

	-- Behaviour of component 'or_258' model 'or'
	or_258 <=
		and_259;

	-- Behaviour of component 'and_259' model 'and'
	and_259 <=
		sig_1093;

	-- Behaviour of component 'not_264' model 'not'
	not_264 <= not (
		cp_en
	);

	-- Behaviour of component 'or_266' model 'or'
	or_266 <=
		and_267;

	-- Behaviour of component 'and_267' model 'and'
	and_267 <=
		sig_1084;

	-- Behaviour of component 'or_272' model 'or'
	or_272 <=
		and_273;

	-- Behaviour of component 'and_273' model 'and'
	and_273 <=
		sig_1084;

	-- Behaviour of component 'or_280' model 'or'
	or_280 <=
		and_281;

	-- Behaviour of component 'and_281' model 'and'
	and_281 <=
		sig_1084;

	-- Behaviour of component 'or_287' model 'or'
	or_287 <=
		and_288;

	-- Behaviour of component 'and_288' model 'and'
	and_288 <=
		sig_1093;

	-- Behaviour of component 'or_293' model 'or'
	or_293 <=
		and_294;

	-- Behaviour of component 'and_294' model 'and'
	and_294 <=
		sig_1093;

	-- Behaviour of component 'or_301' model 'or'
	or_301 <=
		and_302;

	-- Behaviour of component 'and_302' model 'and'
	and_302 <=
		sig_1093;

	-- Behaviour of component 'or_304' model 'or'
	or_304 <=
		and_305;

	-- Behaviour of component 'and_305' model 'and'
	and_305 <=
		sig_1084;

	-- Behaviour of component 'or_306' model 'or'
	or_306 <=
		and_307;

	-- Behaviour of component 'and_307' model 'and'
	and_307 <=
		sig_1084;

	-- Behaviour of component 'or_308' model 'or'
	or_308 <=
		and_309;

	-- Behaviour of component 'and_309' model 'and'
	and_309 <=
		sig_1084;

	-- Behaviour of component 'or_312' model 'or'
	or_312 <=
		and_313;

	-- Behaviour of component 'and_313' model 'and'
	and_313 <=
		sig_1084;

	-- Behaviour of component 'or_318' model 'or'
	or_318 <=
		and_319;

	-- Behaviour of component 'and_319' model 'and'
	and_319 <=
		sig_1084;

	-- Behaviour of component 'or_329' model 'or'
	or_329 <=
		and_330;

	-- Behaviour of component 'and_330' model 'and'
	and_330 <=
		sig_1094;

	-- Behaviour of component 'or_335' model 'or'
	or_335 <=
		and_336;

	-- Behaviour of component 'and_336' model 'and'
	and_336 <=
		sig_1095;

	-- Behaviour of component 'or_339' model 'or'
	or_339 <=
		and_340;

	-- Behaviour of component 'and_340' model 'and'
	and_340 <=
		sig_1094;

	-- Behaviour of component 'or_342' model 'or'
	or_342 <=
		and_343;

	-- Behaviour of component 'and_343' model 'and'
	and_343 <=
		sig_1095;

	-- Behaviour of component 'or_346' model 'or'
	or_346 <=
		and_347;

	-- Behaviour of component 'and_347' model 'and'
	and_347 <=
		sig_1084;

	-- Behaviour of component 'or_348' model 'or'
	or_348 <=
		and_349;

	-- Behaviour of component 'and_349' model 'and'
	and_349 <=
		sig_1085;

	-- Behaviour of component 'or_351' model 'or'
	or_351 <=
		and_352;

	-- Behaviour of component 'and_352' model 'and'
	and_352 <=
		sig_1083(30 downto 0);

	-- Behaviour of component 'or_355' model 'or'
	or_355 <=
		and_356;

	-- Behaviour of component 'and_356' model 'and'
	and_356 <=
		sig_1086(30 downto 0);

	-- Behaviour of component 'or_360' model 'or'
	or_360 <=
		and_361;

	-- Behaviour of component 'and_361' model 'and'
	and_361 <=
		sig_1094;

	-- Behaviour of component 'or_371' model 'or'
	or_371 <=
		and_372;

	-- Behaviour of component 'and_372' model 'and'
	and_372 <=
		sig_1093;

	-- Behaviour of component 'or_378' model 'or'
	or_378 <=
		and_379;

	-- Behaviour of component 'and_379' model 'and'
	and_379 <=
		sig_1092;

	-- Behaviour of component 'or_380' model 'or'
	or_380 <=
		and_381;

	-- Behaviour of component 'and_381' model 'and'
	and_381 <=
		sig_1095;

	-- Behaviour of component 'or_384' model 'or'
	or_384 <=
		and_385;

	-- Behaviour of component 'and_385' model 'and'
	and_385 <=
		sig_1084;

	-- Behaviour of component 'or_386' model 'or'
	or_386 <=
		and_387;

	-- Behaviour of component 'and_387' model 'and'
	and_387 <=
		sig_1084;

	-- Behaviour of component 'or_388' model 'or'
	or_388 <=
		and_389;

	-- Behaviour of component 'and_389' model 'and'
	and_389 <=
		sig_1085;

	-- Behaviour of component 'or_394' model 'or'
	or_394 <=
		and_395 or
		and_399 or
		and_401 or
		and_403 or
		and_405 or
		and_397 or
		and_407 or
		and_409;

	-- Behaviour of component 'and_395' model 'and'
	and_395 <=
		sig_1096 and
		repeat(8, sig_674);

	-- Behaviour of component 'and_397' model 'and'
	and_397 <=
		sig_1097 and
		repeat(8, sig_1070);

	-- Behaviour of component 'and_399' model 'and'
	and_399 <=
		sig_1098 and
		repeat(8, sig_1069);

	-- Behaviour of component 'and_401' model 'and'
	and_401 <=
		sig_1099 and
		repeat(8, sig_673);

	-- Behaviour of component 'or_414' model 'or'
	or_414 <=
		and_415;

	-- Behaviour of component 'or_423' model 'or'
	or_423 <=
		and_424;

	-- Behaviour of component 'and_424' model 'and'
	and_424 <=
		sig_1084;

	-- Behaviour of component 'or_425' model 'or'
	or_425 <=
		and_426;

	-- Behaviour of component 'and_426' model 'and'
	and_426 <=
		sig_1093;

	-- Behaviour of component 'or_427' model 'or'
	or_427 <=
		and_428;

	-- Behaviour of component 'and_428' model 'and'
	and_428 <=
		sig_1085;

	-- Behaviour of component 'or_431' model 'or'
	or_431 <=
		and_432;

	-- Behaviour of component 'and_432' model 'and'
	and_432 <=
		sig_1093;

	-- Behaviour of component 'or_433' model 'or'
	or_433 <=
		and_434;

	-- Behaviour of component 'and_434' model 'and'
	and_434 <=
		sig_1084;

	-- Behaviour of component 'or_438' model 'or'
	or_438 <=
		and_439;

	-- Behaviour of component 'and_439' model 'and'
	and_439 <=
		sig_1093;

	-- Behaviour of component 'or_440' model 'or'
	or_440 <=
		and_441;

	-- Behaviour of component 'and_441' model 'and'
	and_441 <=
		sig_1094;

	-- Behaviour of component 'or_443' model 'or'
	or_443 <=
		and_444;

	-- Behaviour of component 'and_444' model 'and'
	and_444 <=
		sig_1092;

	-- Behaviour of component 'or_450' model 'or'
	or_450 <=
		and_451;

	-- Behaviour of component 'and_451' model 'and'
	and_451 <=
		sig_1093;

	-- Behaviour of component 'or_454' model 'or'
	or_454 <=
		and_455;

	-- Behaviour of component 'and_455' model 'and'
	and_455 <=
		sig_1084(30 downto 0);

	-- Behaviour of component 'or_458' model 'or'
	or_458 <=
		and_459;

	-- Behaviour of component 'and_459' model 'and'
	and_459 <=
		sig_1094;

	-- Behaviour of component 'or_462' model 'or'
	or_462 <=
		and_463;

	-- Behaviour of component 'and_463' model 'and'
	and_463 <=
		sig_1092;

	-- Behaviour of component 'or_467' model 'or'
	or_467 <=
		and_468;

	-- Behaviour of component 'and_468' model 'and'
	and_468 <=
		sig_1084(30 downto 0);

	-- Behaviour of component 'or_475' model 'or'
	or_475 <=
		and_476;

	-- Behaviour of component 'and_476' model 'and'
	and_476 <=
		sig_1086(30 downto 0);

	-- Behaviour of component 'or_479' model 'or'
	or_479 <=
		and_480;

	-- Behaviour of component 'and_480' model 'and'
	and_480 <=
		sig_1093;

	-- Behaviour of component 'or_481' model 'or'
	or_481 <=
		and_482;

	-- Behaviour of component 'and_482' model 'and'
	and_482 <=
		sig_1094;

	-- Behaviour of component 'or_485' model 'or'
	or_485 <=
		and_486;

	-- Behaviour of component 'and_486' model 'and'
	and_486 <=
		sig_1094;

	-- Behaviour of component 'or_490' model 'or'
	or_490 <=
		and_491;

	-- Behaviour of component 'and_491' model 'and'
	and_491 <=
		sig_1094;

	-- Behaviour of component 'or_493' model 'or'
	or_493 <=
		and_494;

	-- Behaviour of component 'or_497' model 'or'
	or_497 <=
		and_498;

	-- Behaviour of component 'or_512' model 'or'
	or_512 <=
		and_513;

	-- Behaviour of component 'and_513' model 'and'
	and_513 <=
		sig_1085;

	-- Behaviour of component 'or_518' model 'or'
	or_518 <=
		and_519;

	-- Behaviour of component 'and_519' model 'and'
	and_519 <=
		sig_1086(30 downto 0);

	-- Behaviour of component 'or_525' model 'or'
	or_525 <=
		and_526;

	-- Behaviour of component 'and_526' model 'and'
	and_526 <=
		sig_1092;

	-- Behaviour of component 'or_529' model 'or'
	or_529 <=
		and_530;

	-- Behaviour of component 'and_530' model 'and'
	and_530 <=
		sig_1085(30 downto 0);

	-- Behaviour of component 'or_532' model 'or'
	or_532 <=
		and_533;

	-- Behaviour of component 'and_533' model 'and'
	and_533 <=
		sig_1085(30 downto 0);

	-- Behaviour of component 'or_535' model 'or'
	or_535 <=
		and_536;

	-- Behaviour of component 'and_536' model 'and'
	and_536 <=
		sig_1094;

	-- Behaviour of component 'or_538' model 'or'
	or_538 <=
		and_539;

	-- Behaviour of component 'and_539' model 'and'
	and_539 <=
		sig_1092;

	-- Behaviour of component 'or_541' model 'or'
	or_541 <=
		and_542;

	-- Behaviour of component 'and_542' model 'and'
	and_542 <=
		sig_1095;

	-- Behaviour of component 'or_545' model 'or'
	or_545 <=
		and_546;

	-- Behaviour of component 'and_546' model 'and'
	and_546 <=
		sig_1084(30 downto 0);

	-- Behaviour of component 'or_548' model 'or'
	or_548 <=
		and_549;

	-- Behaviour of component 'and_549' model 'and'
	and_549 <=
		sig_1085;

	-- Behaviour of component 'or_554' model 'or'
	or_554 <=
		and_555;

	-- Behaviour of component 'and_555' model 'and'
	and_555 <=
		sig_1086(30 downto 0);

	-- Behaviour of component 'or_557' model 'or'
	or_557 <=
		and_558;

	-- Behaviour of component 'and_558' model 'and'
	and_558 <=
		sig_1085(30 downto 0);

	-- Behaviour of component 'or_568' model 'or'
	or_568 <=
		and_569;

	-- Behaviour of component 'and_569' model 'and'
	and_569 <=
		sig_1085;

	-- Behaviour of component 'or_571' model 'or'
	or_571 <=
		and_572;

	-- Behaviour of component 'and_572' model 'and'
	and_572 <=
		sig_1083(30 downto 0);

	-- Behaviour of component 'or_575' model 'or'
	or_575 <=
		and_576;

	-- Behaviour of component 'and_576' model 'and'
	and_576 <=
		sig_1086(30 downto 0);

	-- Behaviour of component 'or_590' model 'or'
	or_590 <=
		and_591;

	-- Behaviour of component 'and_591' model 'and'
	and_591 <=
		sig_1094;

	-- Behaviour of component 'or_597' model 'or'
	or_597 <=
		and_598;

	-- Behaviour of component 'and_598' model 'and'
	and_598 <=
		sig_1085;

	-- Behaviour of component 'or_603' model 'or'
	or_603 <=
		and_604;

	-- Behaviour of component 'and_604' model 'and'
	and_604 <=
		sig_1084(30 downto 0);

	-- Behaviour of all components of model 'reg'
	-- Registers with clock = sig_clock and reset = sig_reset active '1'
	process(sig_clock, sig_reset)
	begin
		if sig_reset = '1' then
			psc_stuff_reg_19 <= "000000000000000000000000000000000000000000000000000000000000000";
		else
			if rising_edge(sig_clock) then
				if mux_28 = '1' then
					psc_stuff_reg_19 <= psc_stuff_reg_18 & write8_u8 & augh_main_k(31 downto 1);
				end if;
			end if;
		end if;
	end process;
	-- Registers with clock = sig_clock and no reset
	process(sig_clock)
	begin
		if rising_edge(sig_clock) then
			if mux_30 = '1' then
				psc_stuff_reg_18 <= cp_din(63 downto 40);
			end if;
			if mux_34 = '1' then
				cp_id_reg_stable_15 <= mux_33;
			end if;
			if mux_36 = '1' then
				cp_id_reg_14 <= mux_35;
			end if;
			if mux_38 = '1' then
				psc_loop_reg_13 <= mux_37;
			end if;
			if sig_1024 = '1' then
				idct_2d_yc_reg7 <= mux_69;
			end if;
			if sig_1025 = '1' then
				idct_2d_yc_reg6 <= mux_71;
			end if;
			if sig_1027 = '1' then
				idct_2d_yc_reg5 <= mux_73;
			end if;
			if sig_1028 = '1' then
				idct_2d_yc_reg4 <= mux_75;
			end if;
			if sig_1029 = '1' then
				idct_2d_yc_reg3 <= mux_77;
			end if;
			if sig_1030 = '1' then
				idct_2d_yc_reg2 <= mux_79;
			end if;
			if sig_1031 = '1' then
				idct_2d_yc_reg1 <= mux_81;
			end if;
			if sig_1032 = '1' then
				idct_2d_yc_reg0 <= mux_83;
			end if;
			if sig_1001 = '1' then
				idct_z2_reg7 <= sig_1090;
			end if;
			if sig_998 = '1' then
				idct_z2_reg6 <= sig_701;
			end if;
			if sig_999 = '1' then
				idct_z2_reg5 <= sig_1113;
			end if;
			if sig_1000 = '1' then
				idct_z2_reg4 <= sig_1120;
			end if;
			if sig_967 = '1' then
				idct_z2_reg3 <= sig_1139;
			end if;
			if sig_968 = '1' then
				idct_z2_reg2 <= sig_1140;
			end if;
			if sig_969 = '1' then
				idct_z2_reg1 <= sig_1141;
			end if;
			if sig_970 = '1' then
				idct_z2_reg0 <= sig_1110;
			end if;
			if sig_971 = '1' then
				idct_z1_reg7 <= mux_109;
			end if;
			if sig_959 = '1' then
				idct_z1_reg6 <= mux_111;
			end if;
			if sig_988 = '1' then
				idct_z1_reg5 <= mux_113;
			end if;
			if sig_971 = '1' then
				idct_z1_reg4 <= mux_115;
			end if;
			if sig_964 = '1' then
				idct_z1_reg3 <= mux_117;
			end if;
			if sig_962 = '1' then
				idct_z1_reg2 <= mux_119;
			end if;
			if sig_992 = '1' then
				idct_z1_reg1 <= mux_121;
			end if;
			if sig_994 = '1' then
				idct_z1_reg0 <= mux_123;
			end if;
			if sig_1023 = '1' then
				idct_z3_reg7 <= sig_1147 & idct_z2_reg7(0);
			end if;
			if sig_1022 = '1' then
				idct_z3_reg6 <= sig_1091 & idct_z2_reg6(1 downto 0);
			end if;
			if sig_1020 = '1' then
				idct_z3_reg5 <= mux_129;
			end if;
			if sig_1019 = '1' then
				idct_z3_reg4 <= sig_1121;
			end if;
			if mux_134 = '1' then
				write8_u8 <= mux_133;
			end if;
			if sig_1046 = '1' then
				idct_2d_r <= mux_135;
			end if;
			if mux_147 = '1' then
				read32_ret0_10 <= stdin_data;
			end if;
			if mux_150 = '1' then
				augh_main_k <= mux_149;
			end if;
		end if;
	end process;

	-- Remaining signal assignments
	-- Those who are not assigned by component instantiation

	sig_clock <= clock;
	sig_reset <= reset;
	sig_start <= start;
	test_cp_0_16 <= mux_32;
	sig_1160 <= sig_608(26) & sig_608(26 downto 8);
	sig_1161 <= sig_1122 & "00";
	sig_1162 <= sig_1122 & "00";
	sig_1163 <= sig_1107(26) & sig_1107(26 downto 8);
	sig_1164 <= sig_1122 & "00";
	sig_1165 <= sig_696(26) & sig_696(26 downto 8);
	sig_1166 <= sig_1088(26) & sig_1088(26 downto 8);
	sig_1167 <= sig_1108(26) & sig_1108(26 downto 8);
	sig_1168 <= sig_670(26) & sig_670(26 downto 8);
	sig_1169 <= sig_1089(26) & sig_1089(26 downto 8);
	sig_1170 <= sig_1122 & "00";
	sig_1171 <= sig_1117(26) & sig_1117(26 downto 8);
	sig_1172 <= sig_720(26) & sig_720(26 downto 8);
	sig_1173 <= sig_1155(26) & sig_1155(26 downto 8);
	sig_1174 <= sig_1122 & "00";
	sig_1175 <= sig_1122 & "00";
	sig_1176 <= sig_1122 & "00";
	sig_1177 <= sig_1122 & "00";
	sig_1178 <= sig_1143 & '0';
	sig_1179 <= sig_1116(26) & sig_1116(26 downto 8);
	sig_1180 <= sig_1104(26) & sig_1104(26 downto 8);
	sig_1181 <= sig_719(26) & sig_719(26 downto 8);
	sig_1182 <= sig_718(26) & sig_718(26 downto 8);
	sig_1183 <= sig_1118(26) & sig_1118(26 downto 8);

	-- Remaining top-level ports assignments
	-- Those who are not assigned by component instantiation

	cp_ok <= mux_151;
	cp_dout <= mux_152;
	stdout_data <= write8_u8;
	stdout_rdy <= mux_154;
	stdin_rdy <= mux_155;

end architecture;
