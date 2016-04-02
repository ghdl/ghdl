library ieee;
use ieee.std_logic_1164.all;

entity top is
	port (
		clock : in  std_logic;
		reset : in  std_logic;
		start : in  std_logic;
		stdin_data : in  std_logic_vector(31 downto 0);
		stdin_rdy : out std_logic;
		stdin_ack : in  std_logic;
		stdout_data : out std_logic_vector(31 downto 0);
		stdout_rdy : out std_logic;
		stdout_ack : in  std_logic;
		cp_en : in  std_logic;
		cp_rest : in  std_logic;
		cp_din : in  std_logic_vector(63 downto 0);
		cp_dout : out std_logic_vector(63 downto 0);
		cp_ok : out std_logic
	);
end top;

architecture augh of top is

	-- Declaration of components

	component v_split0 is
		port (
			clk : in  std_logic;
			ra0_data : out std_logic_vector(7 downto 0);
			wa0_data : in  std_logic_vector(7 downto 0);
			wa0_addr : in  std_logic;
			wa0_en : in  std_logic;
			ra0_addr : in  std_logic
		);
	end component;

	component v_split1 is
		port (
			clk : in  std_logic;
			ra0_data : out std_logic_vector(7 downto 0);
			wa0_data : in  std_logic_vector(7 downto 0);
			wa0_addr : in  std_logic;
			wa0_en : in  std_logic;
			ra0_addr : in  std_logic
		);
	end component;

	component v_split2 is
		port (
			clk : in  std_logic;
			ra0_data : out std_logic_vector(7 downto 0);
			wa0_data : in  std_logic_vector(7 downto 0);
			wa0_addr : in  std_logic;
			wa0_en : in  std_logic;
			ra0_addr : in  std_logic
		);
	end component;

	component v_split3 is
		port (
			clk : in  std_logic;
			ra0_data : out std_logic_vector(7 downto 0);
			wa0_data : in  std_logic_vector(7 downto 0);
			wa0_addr : in  std_logic;
			wa0_en : in  std_logic;
			ra0_addr : in  std_logic
		);
	end component;

	component v_split4 is
		port (
			clk : in  std_logic;
			ra0_data : out std_logic_vector(7 downto 0);
			wa0_data : in  std_logic_vector(7 downto 0);
			wa0_addr : in  std_logic;
			wa0_en : in  std_logic;
			ra0_addr : in  std_logic
		);
	end component;

	component v_split5 is
		port (
			clk : in  std_logic;
			ra0_data : out std_logic_vector(7 downto 0);
			wa0_data : in  std_logic_vector(7 downto 0);
			wa0_addr : in  std_logic;
			wa0_en : in  std_logic;
			ra0_addr : in  std_logic
		);
	end component;

	component v_split6 is
		port (
			clk : in  std_logic;
			ra0_data : out std_logic_vector(7 downto 0);
			wa0_data : in  std_logic_vector(7 downto 0);
			wa0_addr : in  std_logic;
			wa0_en : in  std_logic;
			ra0_addr : in  std_logic
		);
	end component;

	component v_split7 is
		port (
			clk : in  std_logic;
			ra0_data : out std_logic_vector(7 downto 0);
			wa0_data : in  std_logic_vector(7 downto 0);
			wa0_addr : in  std_logic;
			wa0_en : in  std_logic;
			ra0_addr : in  std_logic
		);
	end component;

	component w_split0 is
		port (
			clk : in  std_logic;
			ra0_data : out std_logic_vector(7 downto 0);
			wa0_data : in  std_logic_vector(7 downto 0);
			wa0_addr : in  std_logic;
			wa0_en : in  std_logic;
			ra0_addr : in  std_logic
		);
	end component;

	component w_split1 is
		port (
			clk : in  std_logic;
			ra0_data : out std_logic_vector(7 downto 0);
			wa0_data : in  std_logic_vector(7 downto 0);
			wa0_addr : in  std_logic;
			wa0_en : in  std_logic;
			ra0_addr : in  std_logic
		);
	end component;

	component w_split2 is
		port (
			clk : in  std_logic;
			ra0_data : out std_logic_vector(7 downto 0);
			wa0_data : in  std_logic_vector(7 downto 0);
			wa0_addr : in  std_logic;
			wa0_en : in  std_logic;
			ra0_addr : in  std_logic
		);
	end component;

	component w_split3 is
		port (
			clk : in  std_logic;
			ra0_data : out std_logic_vector(7 downto 0);
			wa0_data : in  std_logic_vector(7 downto 0);
			wa0_addr : in  std_logic;
			wa0_en : in  std_logic;
			ra0_addr : in  std_logic
		);
	end component;

	component w_split4 is
		port (
			clk : in  std_logic;
			ra0_data : out std_logic_vector(7 downto 0);
			wa0_data : in  std_logic_vector(7 downto 0);
			wa0_addr : in  std_logic;
			wa0_en : in  std_logic;
			ra0_addr : in  std_logic
		);
	end component;

	component w_split5 is
		port (
			clk : in  std_logic;
			ra0_data : out std_logic_vector(7 downto 0);
			wa0_data : in  std_logic_vector(7 downto 0);
			wa0_addr : in  std_logic;
			wa0_en : in  std_logic;
			ra0_addr : in  std_logic
		);
	end component;

	component w_split6 is
		port (
			clk : in  std_logic;
			ra0_data : out std_logic_vector(7 downto 0);
			wa0_data : in  std_logic_vector(7 downto 0);
			wa0_addr : in  std_logic;
			wa0_en : in  std_logic;
			ra0_addr : in  std_logic
		);
	end component;

	component w_split7 is
		port (
			clk : in  std_logic;
			ra0_data : out std_logic_vector(7 downto 0);
			wa0_data : in  std_logic_vector(7 downto 0);
			wa0_addr : in  std_logic;
			wa0_en : in  std_logic;
			ra0_addr : in  std_logic
		);
	end component;

	component add_171 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component cmp_183 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component cmp_185 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component cmp_193 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component cmp_195 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component muxb_120 is
		port (
			in_sel : in  std_logic;
			out_data : out std_logic;
			in_data0 : in  std_logic;
			in_data1 : in  std_logic
		);
	end component;

	component muxb_124 is
		port (
			in_sel : in  std_logic;
			out_data : out std_logic;
			in_data0 : in  std_logic;
			in_data1 : in  std_logic
		);
	end component;

	component cmp_128 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component cmp_130 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component cmp_132 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component cmp_136 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component cmp_137 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component cmp_138 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component cmp_139 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component cmp_140 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component cmp_141 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component cmp_142 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component muxb_117 is
		port (
			in_sel : in  std_logic;
			out_data : out std_logic;
			in_data0 : in  std_logic;
			in_data1 : in  std_logic
		);
	end component;

	component add_118 is
		port (
			result : out std_logic_vector(15 downto 0);
			in_a : in  std_logic_vector(15 downto 0);
			in_b : in  std_logic_vector(15 downto 0)
		);
	end component;

	component cmp_119 is
		port (
			ne : out std_logic;
			in0 : in  std_logic_vector(15 downto 0);
			in1 : in  std_logic_vector(15 downto 0)
		);
	end component;

	component muxb_121 is
		port (
			in_sel : in  std_logic;
			out_data : out std_logic;
			in_data0 : in  std_logic;
			in_data1 : in  std_logic
		);
	end component;

	component muxb_123 is
		port (
			in_sel : in  std_logic;
			out_data : out std_logic;
			in_data0 : in  std_logic;
			in_data1 : in  std_logic
		);
	end component;

	component sub_125 is
		port (
			le : out std_logic;
			sign : in  std_logic;
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component cmp_126 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component add_134 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component sub_145 is
		port (
			result : out std_logic_vector(3 downto 0);
			in_a : in  std_logic_vector(3 downto 0);
			in_b : in  std_logic_vector(3 downto 0)
		);
	end component;

	component cmp_146 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component add_170 is
		port (
			result : out std_logic_vector(8 downto 0);
			in_a : in  std_logic_vector(8 downto 0);
			in_b : in  std_logic_vector(8 downto 0)
		);
	end component;

	component cmp_174 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component cmp_176 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component cmp_178 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component cmp_180 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component cmp_187 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component add_188 is
		port (
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component sub_189 is
		port (
			lt : out std_logic;
			sign : in  std_logic;
			result : out std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0)
		);
	end component;

	component cmp_191 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component cmp_198 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component cmp_200 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component cmp_202 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component cmp_204 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component add_214 is
		port (
			result : out std_logic_vector(7 downto 0);
			in_a : in  std_logic_vector(7 downto 0);
			in_b : in  std_logic_vector(7 downto 0)
		);
	end component;

	component cmp_215 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component muxb_219 is
		port (
			in_sel : in  std_logic;
			out_data : out std_logic;
			in_data0 : in  std_logic;
			in_data1 : in  std_logic
		);
	end component;

	component add_220 is
		port (
			result : out std_logic_vector(15 downto 0);
			in_a : in  std_logic_vector(15 downto 0);
			in_b : in  std_logic_vector(15 downto 0)
		);
	end component;

	component cmp_221 is
		port (
			ne : out std_logic;
			in0 : in  std_logic_vector(15 downto 0);
			in1 : in  std_logic_vector(15 downto 0)
		);
	end component;

	component cmp_111 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component cmp_113 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component cmp_216 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component cmp_217 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component cmp_218 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component fsm_15 is
		port (
			clock : in  std_logic;
			reset : in  std_logic;
			out3 : out std_logic;
			out157 : out std_logic;
			out159 : out std_logic;
			out160 : out std_logic;
			out171 : out std_logic;
			out172 : out std_logic;
			out173 : out std_logic;
			out175 : out std_logic;
			out178 : out std_logic;
			in0 : in  std_logic;
			out0 : out std_logic;
			in5 : in  std_logic;
			in6 : in  std_logic;
			in7 : in  std_logic;
			out35 : out std_logic;
			out39 : out std_logic;
			out40 : out std_logic;
			out41 : out std_logic;
			out44 : out std_logic;
			out46 : out std_logic;
			out140 : out std_logic;
			in8 : in  std_logic;
			in9 : in  std_logic;
			in10 : in  std_logic;
			in11 : in  std_logic;
			in12 : in  std_logic;
			in13 : in  std_logic;
			in14 : in  std_logic;
			out65 : out std_logic;
			in1 : in  std_logic;
			in2 : in  std_logic;
			in3 : in  std_logic;
			in4 : in  std_logic;
			out225 : out std_logic;
			out227 : out std_logic;
			out231 : out std_logic;
			out235 : out std_logic;
			out236 : out std_logic;
			out237 : out std_logic;
			out238 : out std_logic;
			out97 : out std_logic;
			out98 : out std_logic;
			out101 : out std_logic;
			out102 : out std_logic;
			out124 : out std_logic;
			out125 : out std_logic;
			out80 : out std_logic;
			out81 : out std_logic;
			out84 : out std_logic;
			out86 : out std_logic;
			out88 : out std_logic;
			out93 : out std_logic;
			out94 : out std_logic
		);
	end component;

	component cmp_112 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component cmp_114 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component cmp_115 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component cmp_148 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component cmp_150 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component cmp_152 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component cmp_154 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component cmp_156 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component cmp_158 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component cmp_160 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component cmp_127 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component cmp_129 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component cmp_131 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component cmp_133 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component cmp_135 is
		port (
			eq : out std_logic;
			in0 : in  std_logic_vector(2 downto 0);
			in1 : in  std_logic_vector(2 downto 0)
		);
	end component;

	-- Declaration of signals

	signal sig_clock : std_logic;
	signal sig_reset : std_logic;
	signal sig_222 : std_logic;
	signal sig_223 : std_logic;
	signal sig_224 : std_logic;
	signal sig_225 : std_logic;
	signal sig_226 : std_logic;
	signal sig_227 : std_logic;
	signal sig_228 : std_logic;
	signal sig_229 : std_logic;
	signal sig_230 : std_logic;
	signal sig_231 : std_logic;
	signal sig_232 : std_logic;
	signal sig_233 : std_logic;
	signal sig_234 : std_logic;
	signal sig_235 : std_logic;
	signal sig_236 : std_logic;
	signal sig_237 : std_logic;
	signal sig_238 : std_logic;
	signal sig_239 : std_logic;
	signal sig_240 : std_logic;
	signal sig_241 : std_logic;
	signal sig_242 : std_logic;
	signal sig_243 : std_logic;
	signal sig_244 : std_logic;
	signal sig_245 : std_logic;
	signal sig_246 : std_logic;
	signal sig_247 : std_logic;
	signal sig_248 : std_logic;
	signal sig_249 : std_logic;
	signal sig_250 : std_logic;
	signal sig_251 : std_logic;
	signal sig_252 : std_logic;
	signal sig_253 : std_logic;
	signal sig_254 : std_logic;
	signal sig_255 : std_logic;
	signal sig_256 : std_logic;
	signal sig_257 : std_logic;
	signal sig_258 : std_logic;
	signal sig_259 : std_logic;
	signal sig_260 : std_logic;
	signal sig_261 : std_logic;
	signal sig_262 : std_logic;
	signal sig_263 : std_logic;
	signal sig_264 : std_logic;
	signal sig_265 : std_logic;
	signal sig_266 : std_logic;
	signal sig_267 : std_logic;
	signal sig_268 : std_logic;
	signal sig_269 : std_logic;
	signal sig_270 : std_logic;
	signal sig_271 : std_logic;
	signal sig_272 : std_logic;
	signal sig_273 : std_logic;
	signal sig_274 : std_logic;
	signal sig_275 : std_logic;
	signal sig_276 : std_logic;
	signal sig_277 : std_logic;
	signal sig_278 : std_logic;
	signal augh_test_0 : std_logic;
	signal augh_test_1 : std_logic;
	signal sig_start : std_logic;
	signal test_cp_4_6 : std_logic;
	signal test_cp_3_7 : std_logic;
	signal test_cp_0_8 : std_logic;
	signal test_cp_1_9 : std_logic;
	signal test_cp_2_10 : std_logic;
	signal memextrct_loop_sig_13 : std_logic;
	signal memextrct_loop_sig_14 : std_logic;
	signal psc_loop_sig_12 : std_logic;
	signal sig_279 : std_logic_vector(15 downto 0);
	signal sig_280 : std_logic;
	signal sig_281 : std_logic;
	signal sig_282 : std_logic_vector(7 downto 0);
	signal sig_283 : std_logic;
	signal sig_284 : std_logic;
	signal sig_285 : std_logic;
	signal sig_286 : std_logic;
	signal sig_287 : std_logic;
	signal sig_288 : std_logic_vector(31 downto 0);
	signal sig_289 : std_logic_vector(31 downto 0);
	signal sig_290 : std_logic;
	signal sig_291 : std_logic;
	signal sig_292 : std_logic;
	signal sig_293 : std_logic;
	signal sig_294 : std_logic;
	signal sig_295 : std_logic_vector(8 downto 0);
	signal sig_296 : std_logic;
	signal sig_297 : std_logic_vector(3 downto 0);
	signal sig_298 : std_logic_vector(31 downto 0);
	signal sig_299 : std_logic;
	signal sig_300 : std_logic_vector(31 downto 0);
	signal sig_301 : std_logic;
	signal sig_302 : std_logic;
	signal sig_303 : std_logic;
	signal sig_304 : std_logic_vector(15 downto 0);
	signal sig_305 : std_logic;
	signal sig_306 : std_logic;
	signal sig_307 : std_logic;
	signal sig_308 : std_logic;
	signal sig_309 : std_logic;
	signal sig_310 : std_logic;
	signal sig_311 : std_logic;
	signal sig_312 : std_logic;
	signal sig_313 : std_logic;
	signal sig_314 : std_logic;
	signal sig_315 : std_logic;
	signal sig_316 : std_logic;
	signal sig_317 : std_logic;
	signal sig_318 : std_logic;
	signal sig_319 : std_logic;
	signal sig_320 : std_logic;
	signal sig_321 : std_logic;
	signal sig_322 : std_logic_vector(31 downto 0);
	signal sig_323 : std_logic_vector(7 downto 0);
	signal sig_324 : std_logic_vector(7 downto 0);
	signal sig_325 : std_logic_vector(7 downto 0);
	signal sig_326 : std_logic_vector(7 downto 0);
	signal sig_327 : std_logic_vector(7 downto 0);
	signal sig_328 : std_logic_vector(7 downto 0);
	signal sig_329 : std_logic_vector(7 downto 0);
	signal sig_330 : std_logic_vector(7 downto 0);
	signal sig_331 : std_logic_vector(7 downto 0);
	signal sig_332 : std_logic_vector(7 downto 0);
	signal sig_333 : std_logic_vector(7 downto 0);
	signal sig_334 : std_logic_vector(7 downto 0);
	signal sig_335 : std_logic_vector(7 downto 0);
	signal sig_336 : std_logic_vector(7 downto 0);
	signal sig_337 : std_logic_vector(7 downto 0);
	signal sig_338 : std_logic_vector(7 downto 0);
	signal sig_339 : std_logic_vector(31 downto 0);
	signal sig_340 : std_logic_vector(8 downto 0);
	signal sig_341 : std_logic_vector(8 downto 0);
	signal sig_342 : std_logic_vector(31 downto 0);

	-- Other inlined components

	signal mux_25 : std_logic;
	signal mux_26 : std_logic_vector(2 downto 0);
	signal mux_27 : std_logic;
	signal mux_28 : std_logic_vector(2 downto 0);
	signal mux_29 : std_logic;
	signal mux_30 : std_logic_vector(15 downto 0);
	signal mux_31 : std_logic;
	signal mux_32 : std_logic_vector(7 downto 0);
	signal mux_33 : std_logic;
	signal mux_34 : std_logic;
	signal mux_35 : std_logic;
	signal mux_36 : std_logic_vector(7 downto 0);
	signal mux_37 : std_logic;
	signal mux_38 : std_logic;
	signal mux_39 : std_logic;
	signal mux_40 : std_logic_vector(7 downto 0);
	signal mux_41 : std_logic;
	signal mux_42 : std_logic;
	signal mux_43 : std_logic;
	signal augh_main_max_iter : std_logic_vector(31 downto 0) := (others => '0');
	signal augh_main_std_addition : std_logic_vector(31 downto 0) := (others => '0');
	signal augh_main_result : std_logic_vector(31 downto 0) := (others => '0');
	signal augh_main_i : std_logic_vector(31 downto 0) := (others => '0');
	signal mux_23 : std_logic;
	signal mux_24 : std_logic;
	signal mux_44 : std_logic_vector(7 downto 0);
	signal mux_45 : std_logic;
	signal mux_46 : std_logic;
	signal mux_47 : std_logic;
	signal mux_48 : std_logic_vector(7 downto 0);
	signal mux_49 : std_logic;
	signal mux_50 : std_logic;
	signal mux_51 : std_logic;
	signal mux_52 : std_logic_vector(7 downto 0);
	signal mux_53 : std_logic;
	signal mux_54 : std_logic;
	signal mux_55 : std_logic;
	signal mux_56 : std_logic_vector(7 downto 0);
	signal mux_57 : std_logic;
	signal mux_58 : std_logic;
	signal mux_59 : std_logic;
	signal mux_60 : std_logic_vector(7 downto 0);
	signal mux_61 : std_logic;
	signal mux_62 : std_logic;
	signal mux_63 : std_logic;
	signal mux_64 : std_logic_vector(31 downto 0);
	signal mux_65 : std_logic;
	signal mux_66 : std_logic_vector(31 downto 0);
	signal mux_67 : std_logic;
	signal mux_68 : std_logic_vector(31 downto 0);
	signal mux_69 : std_logic;
	signal mux_70 : std_logic_vector(31 downto 0);
	signal mux_71 : std_logic;
	signal mux_72 : std_logic_vector(7 downto 0);
	signal mux_73 : std_logic;
	signal mux_74 : std_logic;
	signal mux_75 : std_logic;
	signal mux_76 : std_logic_vector(7 downto 0);
	signal mux_77 : std_logic;
	signal mux_78 : std_logic;
	signal mux_79 : std_logic;
	signal mux_80 : std_logic_vector(7 downto 0);
	signal mux_20 : std_logic;
	signal mux_22 : std_logic;
	signal and_163 : std_logic_vector(7 downto 0);
	signal and_164 : std_logic_vector(7 downto 0);
	signal and_165 : std_logic_vector(7 downto 0);
	signal and_166 : std_logic_vector(7 downto 0);
	signal and_167 : std_logic_vector(7 downto 0);
	signal and_168 : std_logic_vector(7 downto 0);
	signal and_169 : std_logic_vector(7 downto 0);
	signal and_182 : std_logic_vector(7 downto 0);
	signal and_184 : std_logic_vector(7 downto 0);
	signal and_186 : std_logic_vector(7 downto 0);
	signal and_192 : std_logic;
	signal and_194 : std_logic;
	signal and_196 : std_logic;
	signal mux_81 : std_logic;
	signal mux_82 : std_logic;
	signal mux_83 : std_logic;
	signal mux_84 : std_logic_vector(7 downto 0);
	signal mux_85 : std_logic;
	signal mux_86 : std_logic;
	signal mux_87 : std_logic;
	signal mux_88 : std_logic_vector(7 downto 0);
	signal mux_89 : std_logic;
	signal mux_90 : std_logic;
	signal mux_91 : std_logic;
	signal mux_92 : std_logic_vector(7 downto 0);
	signal mux_93 : std_logic;
	signal mux_94 : std_logic;
	signal mux_95 : std_logic;
	signal mux_96 : std_logic_vector(7 downto 0);
	signal mux_97 : std_logic;
	signal mux_98 : std_logic;
	signal mux_99 : std_logic;
	signal mux_100 : std_logic_vector(7 downto 0);
	signal mux_101 : std_logic;
	signal mux_102 : std_logic;
	signal mux_103 : std_logic;
	signal mux_106 : std_logic;
	signal mux_108 : std_logic;
	signal mux_109 : std_logic_vector(63 downto 0);
	signal mux_110 : std_logic;
	signal and_116 : std_logic;
	signal not_122 : std_logic;
	signal or_143 : std_logic_vector(7 downto 0);
	signal and_147 : std_logic_vector(7 downto 0);
	signal and_149 : std_logic_vector(7 downto 0);
	signal and_151 : std_logic_vector(7 downto 0);
	signal and_153 : std_logic_vector(7 downto 0);
	signal and_155 : std_logic_vector(7 downto 0);
	signal and_157 : std_logic_vector(7 downto 0);
	signal and_159 : std_logic_vector(7 downto 0);
	signal or_161 : std_logic_vector(7 downto 0);
	signal and_162 : std_logic_vector(7 downto 0);
	signal or_172 : std_logic_vector(7 downto 0);
	signal and_173 : std_logic_vector(7 downto 0);
	signal and_175 : std_logic_vector(7 downto 0);
	signal and_177 : std_logic_vector(7 downto 0);
	signal and_179 : std_logic_vector(7 downto 0);
	signal and_181 : std_logic_vector(7 downto 0);
	signal and_190 : std_logic;
	signal and_197 : std_logic;
	signal and_199 : std_logic;
	signal and_201 : std_logic;
	signal and_203 : std_logic;
	signal or_205 : std_logic_vector(7 downto 0);
	signal and_206 : std_logic_vector(7 downto 0);
	signal and_207 : std_logic_vector(7 downto 0);
	signal and_208 : std_logic_vector(7 downto 0);
	signal and_209 : std_logic_vector(7 downto 0);
	signal and_210 : std_logic_vector(7 downto 0);
	signal and_211 : std_logic_vector(7 downto 0);
	signal and_212 : std_logic_vector(7 downto 0);
	signal and_213 : std_logic_vector(7 downto 0);
	signal psc_loop_reg_3 : std_logic_vector(15 downto 0) := (others => '0');
	signal cp_id_reg_4 : std_logic_vector(2 downto 0) := (others => '0');
	signal cp_id_reg_stable_5 : std_logic_vector(2 downto 0) := (others => '0');
	signal psc_stuff_reg_11 : std_logic_vector(60 downto 0) := "0000000000000000000000000000000000000000000000000000000000000";
	signal and_144 : std_logic_vector(7 downto 0);

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

	v_split0_i : v_split0 port map (
		clk => sig_clock,
		ra0_data => sig_338,
		wa0_data => mux_100,
		wa0_addr => mux_101,
		wa0_en => mux_102,
		ra0_addr => mux_103
	);

	v_split1_i : v_split1 port map (
		clk => sig_clock,
		ra0_data => sig_337,
		wa0_data => mux_96,
		wa0_addr => mux_97,
		wa0_en => mux_98,
		ra0_addr => mux_99
	);

	v_split2_i : v_split2 port map (
		clk => sig_clock,
		ra0_data => sig_336,
		wa0_data => mux_92,
		wa0_addr => mux_93,
		wa0_en => mux_94,
		ra0_addr => mux_95
	);

	v_split3_i : v_split3 port map (
		clk => sig_clock,
		ra0_data => sig_335,
		wa0_data => mux_88,
		wa0_addr => mux_89,
		wa0_en => mux_90,
		ra0_addr => mux_91
	);

	v_split4_i : v_split4 port map (
		clk => sig_clock,
		ra0_data => sig_334,
		wa0_data => mux_84,
		wa0_addr => mux_85,
		wa0_en => mux_86,
		ra0_addr => mux_87
	);

	v_split5_i : v_split5 port map (
		clk => sig_clock,
		ra0_data => sig_333,
		wa0_data => mux_80,
		wa0_addr => mux_81,
		wa0_en => mux_82,
		ra0_addr => mux_83
	);

	v_split6_i : v_split6 port map (
		clk => sig_clock,
		ra0_data => sig_332,
		wa0_data => mux_76,
		wa0_addr => mux_77,
		wa0_en => mux_78,
		ra0_addr => mux_79
	);

	v_split7_i : v_split7 port map (
		clk => sig_clock,
		ra0_data => sig_331,
		wa0_data => mux_72,
		wa0_addr => mux_73,
		wa0_en => mux_74,
		ra0_addr => mux_75
	);

	w_split0_i : w_split0 port map (
		clk => sig_clock,
		ra0_data => sig_330,
		wa0_data => mux_60,
		wa0_addr => mux_61,
		wa0_en => mux_62,
		ra0_addr => mux_63
	);

	w_split1_i : w_split1 port map (
		clk => sig_clock,
		ra0_data => sig_329,
		wa0_data => mux_56,
		wa0_addr => mux_57,
		wa0_en => mux_58,
		ra0_addr => mux_59
	);

	w_split2_i : w_split2 port map (
		clk => sig_clock,
		ra0_data => sig_328,
		wa0_data => mux_52,
		wa0_addr => mux_53,
		wa0_en => mux_54,
		ra0_addr => mux_55
	);

	w_split3_i : w_split3 port map (
		clk => sig_clock,
		ra0_data => sig_327,
		wa0_data => mux_48,
		wa0_addr => mux_49,
		wa0_en => mux_50,
		ra0_addr => mux_51
	);

	w_split4_i : w_split4 port map (
		clk => sig_clock,
		ra0_data => sig_326,
		wa0_data => mux_44,
		wa0_addr => mux_45,
		wa0_en => mux_46,
		ra0_addr => mux_47
	);

	w_split5_i : w_split5 port map (
		clk => sig_clock,
		ra0_data => sig_325,
		wa0_data => mux_40,
		wa0_addr => mux_41,
		wa0_en => mux_42,
		ra0_addr => mux_43
	);

	w_split6_i : w_split6 port map (
		clk => sig_clock,
		ra0_data => sig_324,
		wa0_data => mux_36,
		wa0_addr => mux_37,
		wa0_en => mux_38,
		ra0_addr => mux_39
	);

	w_split7_i : w_split7 port map (
		clk => sig_clock,
		ra0_data => sig_323,
		wa0_data => mux_32,
		wa0_addr => mux_33,
		wa0_en => mux_34,
		ra0_addr => mux_35
	);

	add_171_i : add_171 port map (
		result => sig_322,
		in_a => sig_342,
		in_b => augh_main_std_addition
	);

	cmp_183_i : cmp_183 port map (
		eq => sig_321,
		in0 => "010",
		in1 => augh_main_i(2 downto 0)
	);

	cmp_185_i : cmp_185 port map (
		eq => sig_320,
		in0 => "001",
		in1 => augh_main_i(2 downto 0)
	);

	cmp_193_i : cmp_193 port map (
		eq => sig_319,
		in0 => "110",
		in1 => augh_main_i(2 downto 0)
	);

	cmp_195_i : cmp_195 port map (
		eq => sig_318,
		in0 => "101",
		in1 => augh_main_i(2 downto 0)
	);

	muxb_120_i : muxb_120 port map (
		in_sel => cp_en,
		out_data => sig_317,
		in_data0 => '0',
		in_data1 => '1'
	);

	muxb_124_i : muxb_124 port map (
		in_sel => not_122,
		out_data => sig_316,
		in_data0 => '0',
		in_data1 => '1'
	);

	cmp_128_i : cmp_128 port map (
		eq => sig_315,
		in0 => "101",
		in1 => augh_main_i(2 downto 0)
	);

	cmp_130_i : cmp_130 port map (
		eq => sig_314,
		in0 => "011",
		in1 => augh_main_i(2 downto 0)
	);

	cmp_132_i : cmp_132 port map (
		eq => sig_313,
		in0 => "001",
		in1 => augh_main_i(2 downto 0)
	);

	cmp_136_i : cmp_136 port map (
		eq => sig_312,
		in0 => "110",
		in1 => sig_298(2 downto 0)
	);

	cmp_137_i : cmp_137 port map (
		eq => sig_311,
		in0 => "101",
		in1 => sig_298(2 downto 0)
	);

	cmp_138_i : cmp_138 port map (
		eq => sig_310,
		in0 => "100",
		in1 => sig_298(2 downto 0)
	);

	cmp_139_i : cmp_139 port map (
		eq => sig_309,
		in0 => "011",
		in1 => sig_298(2 downto 0)
	);

	cmp_140_i : cmp_140 port map (
		eq => sig_308,
		in0 => "010",
		in1 => sig_298(2 downto 0)
	);

	cmp_141_i : cmp_141 port map (
		eq => sig_307,
		in0 => "001",
		in1 => sig_298(2 downto 0)
	);

	cmp_142_i : cmp_142 port map (
		eq => sig_306,
		in0 => "000",
		in1 => sig_298(2 downto 0)
	);

	muxb_117_i : muxb_117 port map (
		in_sel => cp_en,
		out_data => sig_305,
		in_data0 => '0',
		in_data1 => '1'
	);

	add_118_i : add_118 port map (
		result => sig_304,
		in_a => psc_loop_reg_3,
		in_b => "0000000000000001"
	);

	cmp_119_i : cmp_119 port map (
		ne => sig_303,
		in0 => "0000000000000001",
		in1 => psc_loop_reg_3
	);

	muxb_121_i : muxb_121 port map (
		in_sel => not_122,
		out_data => sig_302,
		in_data0 => '0',
		in_data1 => '1'
	);

	muxb_123_i : muxb_123 port map (
		in_sel => not_122,
		out_data => sig_301,
		in_data0 => '0',
		in_data1 => '1'
	);

	sub_125_i : sub_125 port map (
		le => augh_test_1,
		sign => '1',
		result => sig_300,
		in_a => augh_main_i,
		in_b => "00000000000000000000000000000111"
	);

	cmp_126_i : cmp_126 port map (
		eq => sig_299,
		in0 => "111",
		in1 => augh_main_i(2 downto 0)
	);

	add_134_i : add_134 port map (
		result => sig_298,
		in_a => augh_main_i,
		in_b => "00000000000000000000000000000001"
	);

	sub_145_i : sub_145 port map (
		result => sig_297,
		in_a => augh_main_max_iter(3 downto 0),
		in_b => "0001"
	);

	cmp_146_i : cmp_146 port map (
		eq => sig_296,
		in0 => "111",
		in1 => sig_297(2 downto 0)
	);

	add_170_i : add_170 port map (
		result => sig_295,
		in_a => sig_341,
		in_b => sig_340
	);

	cmp_174_i : cmp_174 port map (
		eq => sig_294,
		in0 => "111",
		in1 => augh_main_i(2 downto 0)
	);

	cmp_176_i : cmp_176 port map (
		eq => sig_293,
		in0 => "110",
		in1 => augh_main_i(2 downto 0)
	);

	cmp_178_i : cmp_178 port map (
		eq => sig_292,
		in0 => "101",
		in1 => augh_main_i(2 downto 0)
	);

	cmp_180_i : cmp_180 port map (
		eq => sig_291,
		in0 => "100",
		in1 => augh_main_i(2 downto 0)
	);

	cmp_187_i : cmp_187 port map (
		eq => sig_290,
		in0 => "000",
		in1 => augh_main_i(2 downto 0)
	);

	add_188_i : add_188 port map (
		result => sig_289,
		in_a => augh_main_result,
		in_b => sig_339
	);

	sub_189_i : sub_189 port map (
		lt => augh_test_0,
		sign => '1',
		result => sig_288,
		in_a => augh_main_i,
		in_b => augh_main_max_iter
	);

	cmp_191_i : cmp_191 port map (
		eq => sig_287,
		in0 => "111",
		in1 => augh_main_i(2 downto 0)
	);

	cmp_198_i : cmp_198 port map (
		eq => sig_286,
		in0 => "011",
		in1 => augh_main_i(2 downto 0)
	);

	cmp_200_i : cmp_200 port map (
		eq => sig_285,
		in0 => "010",
		in1 => augh_main_i(2 downto 0)
	);

	cmp_202_i : cmp_202 port map (
		eq => sig_284,
		in0 => "001",
		in1 => augh_main_i(2 downto 0)
	);

	cmp_204_i : cmp_204 port map (
		eq => sig_283,
		in0 => "000",
		in1 => augh_main_i(2 downto 0)
	);

	add_214_i : add_214 port map (
		result => sig_282,
		in_a => or_205,
		in_b => augh_main_i(7 downto 0)
	);

	cmp_215_i : cmp_215 port map (
		eq => sig_281,
		in0 => "001",
		in1 => cp_id_reg_stable_5
	);

	muxb_219_i : muxb_219 port map (
		in_sel => cp_en,
		out_data => sig_280,
		in_data0 => '0',
		in_data1 => '1'
	);

	add_220_i : add_220 port map (
		result => sig_279,
		in_a => psc_loop_reg_3,
		in_b => "0000000000000001"
	);

	cmp_221_i : cmp_221 port map (
		ne => psc_loop_sig_12,
		in0 => "0000000000000010",
		in1 => psc_loop_reg_3
	);

	cmp_111_i : cmp_111 port map (
		eq => test_cp_2_10,
		in0 => "010",
		in1 => cp_id_reg_stable_5
	);

	cmp_113_i : cmp_113 port map (
		eq => sig_278,
		in0 => "000",
		in1 => cp_id_reg_stable_5
	);

	cmp_216_i : cmp_216 port map (
		eq => sig_277,
		in0 => "000",
		in1 => cp_id_reg_stable_5
	);

	cmp_217_i : cmp_217 port map (
		eq => sig_276,
		in0 => "011",
		in1 => cp_id_reg_stable_5
	);

	cmp_218_i : cmp_218 port map (
		eq => sig_275,
		in0 => "100",
		in1 => cp_id_reg_stable_5
	);

	fsm_15_i : fsm_15 port map (
		clock => sig_clock,
		reset => sig_reset,
		out3 => sig_274,
		out157 => sig_273,
		out159 => sig_272,
		out160 => sig_271,
		out171 => sig_270,
		out172 => sig_269,
		out173 => sig_268,
		out175 => sig_267,
		out178 => sig_266,
		in0 => test_cp_4_6,
		out0 => sig_265,
		in5 => memextrct_loop_sig_14,
		in6 => memextrct_loop_sig_13,
		in7 => stdout_ack,
		out35 => sig_264,
		out39 => sig_263,
		out40 => sig_262,
		out41 => sig_261,
		out44 => sig_260,
		out46 => sig_259,
		out140 => sig_258,
		in8 => cp_en,
		in9 => stdin_ack,
		in10 => augh_test_1,
		in11 => augh_test_0,
		in12 => cp_rest,
		in13 => sig_start,
		in14 => psc_loop_sig_12,
		out65 => sig_257,
		in1 => test_cp_3_7,
		in2 => test_cp_0_8,
		in3 => test_cp_1_9,
		in4 => test_cp_2_10,
		out225 => sig_256,
		out227 => sig_255,
		out231 => sig_254,
		out235 => sig_253,
		out236 => sig_252,
		out237 => sig_251,
		out238 => sig_250,
		out97 => sig_249,
		out98 => sig_248,
		out101 => sig_247,
		out102 => sig_246,
		out124 => sig_245,
		out125 => sig_244,
		out80 => sig_243,
		out81 => sig_242,
		out84 => sig_241,
		out86 => sig_240,
		out88 => sig_239,
		out93 => sig_238,
		out94 => sig_237
	);

	cmp_112_i : cmp_112 port map (
		eq => sig_236,
		in0 => "001",
		in1 => cp_id_reg_stable_5
	);

	cmp_114_i : cmp_114 port map (
		eq => sig_235,
		in0 => "011",
		in1 => cp_id_reg_stable_5
	);

	cmp_115_i : cmp_115 port map (
		eq => sig_234,
		in0 => "100",
		in1 => cp_id_reg_stable_5
	);

	cmp_148_i : cmp_148 port map (
		eq => sig_233,
		in0 => "110",
		in1 => sig_297(2 downto 0)
	);

	cmp_150_i : cmp_150 port map (
		eq => sig_232,
		in0 => "101",
		in1 => sig_297(2 downto 0)
	);

	cmp_152_i : cmp_152 port map (
		eq => sig_231,
		in0 => "100",
		in1 => sig_297(2 downto 0)
	);

	cmp_154_i : cmp_154 port map (
		eq => sig_230,
		in0 => "011",
		in1 => sig_297(2 downto 0)
	);

	cmp_156_i : cmp_156 port map (
		eq => sig_229,
		in0 => "010",
		in1 => sig_297(2 downto 0)
	);

	cmp_158_i : cmp_158 port map (
		eq => sig_228,
		in0 => "001",
		in1 => sig_297(2 downto 0)
	);

	cmp_160_i : cmp_160 port map (
		eq => sig_227,
		in0 => "000",
		in1 => sig_297(2 downto 0)
	);

	cmp_127_i : cmp_127 port map (
		eq => sig_226,
		in0 => "110",
		in1 => augh_main_i(2 downto 0)
	);

	cmp_129_i : cmp_129 port map (
		eq => sig_225,
		in0 => "100",
		in1 => augh_main_i(2 downto 0)
	);

	cmp_131_i : cmp_131 port map (
		eq => sig_224,
		in0 => "010",
		in1 => augh_main_i(2 downto 0)
	);

	cmp_133_i : cmp_133 port map (
		eq => sig_223,
		in0 => "000",
		in1 => augh_main_i(2 downto 0)
	);

	cmp_135_i : cmp_135 port map (
		eq => sig_222,
		in0 => "111",
		in1 => sig_298(2 downto 0)
	);

	-- Behaviour of component 'mux_25' model 'mux'
	mux_25 <=
		(sig_274 and sig_234) or
		(sig_254 and sig_275);

	-- Behaviour of component 'mux_26' model 'mux'
	mux_26 <=
		(repeat(3, sig_243) and "010") or
		(repeat(3, sig_253) and cp_din(2 downto 0)) or
		(repeat(3, sig_240) and "001") or
		(repeat(3, sig_271) and "011") or
		(repeat(3, sig_267) and "100");

	-- Behaviour of component 'mux_27' model 'mux'
	mux_27 <=
		(sig_242 and '1') or
		(sig_252 and cp_rest);

	-- Behaviour of component 'mux_28' model 'mux'
	mux_28 <=
		(repeat(3, sig_243) and "010") or
		(repeat(3, sig_251) and augh_main_result(2 downto 0)) or
		(repeat(3, sig_240) and "001") or
		(repeat(3, sig_271) and "011") or
		(repeat(3, sig_267) and "100");

	-- Behaviour of component 'mux_29' model 'mux'
	mux_29 <=
		(sig_242 and '1') or
		(sig_250 and cp_en);

	-- Behaviour of component 'mux_30' model 'mux'
	mux_30 <=
		(repeat(16, sig_262) and sig_304) or
		(repeat(16, sig_251) and sig_279);

	-- Behaviour of component 'mux_31' model 'mux'
	mux_31 <=
		(sig_261 and cp_en) or
		(sig_260 and '1');

	-- Behaviour of component 'mux_32' model 'mux'
	mux_32 <=
		(repeat(8, sig_263) and cp_din(7 downto 0)) or
		(repeat(8, sig_267) and sig_282);

	-- Behaviour of component 'mux_33' model 'mux'
	mux_33 <=
		(sig_263 and psc_loop_reg_3(0)) or
		(sig_267 and augh_main_i(3));

	-- Behaviour of component 'mux_34' model 'mux'
	mux_34 <=
		(sig_264 and and_116) or
		(sig_266 and and_190);

	-- Behaviour of component 'mux_35' model 'mux'
	mux_35 <=
		(sig_263 and psc_loop_reg_3(0)) or
		(sig_258 and sig_297(3)) or
		(sig_267 and augh_main_i(3));

	-- Behaviour of component 'mux_36' model 'mux'
	mux_36 <=
		(repeat(8, sig_263) and cp_din(15 downto 8)) or
		(repeat(8, sig_267) and sig_282);

	-- Behaviour of component 'mux_37' model 'mux'
	mux_37 <=
		(sig_263 and psc_loop_reg_3(0)) or
		(sig_267 and augh_main_i(3));

	-- Behaviour of component 'mux_38' model 'mux'
	mux_38 <=
		(sig_264 and and_116) or
		(sig_266 and and_192);

	-- Behaviour of component 'mux_39' model 'mux'
	mux_39 <=
		(sig_263 and psc_loop_reg_3(0)) or
		(sig_258 and sig_297(3)) or
		(sig_267 and augh_main_i(3));

	-- Behaviour of component 'mux_40' model 'mux'
	mux_40 <=
		(repeat(8, sig_263) and cp_din(23 downto 16)) or
		(repeat(8, sig_267) and sig_282);

	-- Behaviour of component 'mux_41' model 'mux'
	mux_41 <=
		(sig_263 and psc_loop_reg_3(0)) or
		(sig_267 and augh_main_i(3));

	-- Behaviour of component 'mux_42' model 'mux'
	mux_42 <=
		(sig_264 and and_116) or
		(sig_266 and and_194);

	-- Behaviour of component 'mux_43' model 'mux'
	mux_43 <=
		(sig_263 and psc_loop_reg_3(0)) or
		(sig_258 and sig_297(3)) or
		(sig_267 and augh_main_i(3));

	-- Behaviour of component 'mux_23' model 'mux'
	mux_23 <=
		(sig_274 and sig_278) or
		(sig_254 and sig_277);

	-- Behaviour of component 'mux_24' model 'mux'
	mux_24 <=
		(sig_274 and sig_235) or
		(sig_254 and sig_276);

	-- Behaviour of component 'mux_44' model 'mux'
	mux_44 <=
		(repeat(8, sig_263) and cp_din(31 downto 24)) or
		(repeat(8, sig_267) and sig_282);

	-- Behaviour of component 'mux_45' model 'mux'
	mux_45 <=
		(sig_263 and psc_loop_reg_3(0)) or
		(sig_249 and '1') or
		(sig_267 and augh_main_i(3));

	-- Behaviour of component 'mux_46' model 'mux'
	mux_46 <=
		(sig_264 and and_116) or
		(sig_248 and '1') or
		(sig_266 and and_196);

	-- Behaviour of component 'mux_47' model 'mux'
	mux_47 <=
		(sig_263 and psc_loop_reg_3(0)) or
		(sig_258 and sig_297(3)) or
		(sig_267 and augh_main_i(3));

	-- Behaviour of component 'mux_48' model 'mux'
	mux_48 <=
		(repeat(8, sig_263) and cp_din(39 downto 32)) or
		(repeat(8, sig_267) and sig_282);

	-- Behaviour of component 'mux_49' model 'mux'
	mux_49 <=
		(sig_263 and psc_loop_reg_3(0)) or
		(sig_267 and augh_main_i(3));

	-- Behaviour of component 'mux_50' model 'mux'
	mux_50 <=
		(sig_264 and and_116) or
		(sig_266 and and_197);

	-- Behaviour of component 'mux_51' model 'mux'
	mux_51 <=
		(sig_263 and psc_loop_reg_3(0)) or
		(sig_258 and sig_297(3)) or
		(sig_267 and augh_main_i(3));

	-- Behaviour of component 'mux_52' model 'mux'
	mux_52 <=
		(repeat(8, sig_263) and cp_din(47 downto 40)) or
		(repeat(8, sig_267) and sig_282);

	-- Behaviour of component 'mux_53' model 'mux'
	mux_53 <=
		(sig_263 and psc_loop_reg_3(0)) or
		(sig_267 and augh_main_i(3));

	-- Behaviour of component 'mux_54' model 'mux'
	mux_54 <=
		(sig_264 and and_116) or
		(sig_266 and and_199);

	-- Behaviour of component 'mux_55' model 'mux'
	mux_55 <=
		(sig_263 and psc_loop_reg_3(0)) or
		(sig_258 and sig_297(3)) or
		(sig_267 and augh_main_i(3));

	-- Behaviour of component 'mux_56' model 'mux'
	mux_56 <=
		(repeat(8, sig_263) and cp_din(55 downto 48)) or
		(repeat(8, sig_267) and sig_282) or
		(repeat(8, sig_256) and "00000011");

	-- Behaviour of component 'mux_57' model 'mux'
	mux_57 <=
		(sig_263 and psc_loop_reg_3(0)) or
		(sig_267 and augh_main_i(3));

	-- Behaviour of component 'mux_58' model 'mux'
	mux_58 <=
		(sig_264 and and_116) or
		(sig_266 and and_201) or
		(sig_255 and '1');

	-- Behaviour of component 'mux_59' model 'mux'
	mux_59 <=
		(sig_263 and psc_loop_reg_3(0)) or
		(sig_258 and sig_297(3)) or
		(sig_267 and augh_main_i(3));

	-- Behaviour of component 'mux_60' model 'mux'
	mux_60 <=
		(repeat(8, sig_263) and cp_din(63 downto 56)) or
		(repeat(8, sig_267) and sig_282);

	-- Behaviour of component 'mux_61' model 'mux'
	mux_61 <=
		(sig_263 and psc_loop_reg_3(0)) or
		(sig_267 and augh_main_i(3));

	-- Behaviour of component 'mux_62' model 'mux'
	mux_62 <=
		(sig_264 and and_116) or
		(sig_266 and and_203);

	-- Behaviour of component 'mux_63' model 'mux'
	mux_63 <=
		(sig_263 and psc_loop_reg_3(0)) or
		(sig_258 and sig_297(3)) or
		(sig_267 and augh_main_i(3));

	-- Behaviour of component 'mux_64' model 'mux'
	mux_64 <=
		(repeat(32, sig_269) and sig_298) or
		(repeat(32, sig_251) and augh_main_std_addition);

	-- Behaviour of component 'mux_65' model 'mux'
	mux_65 <=
		(sig_272 and '1') or
		(sig_268 and not_122) or
		(sig_250 and cp_en);

	-- Behaviour of component 'mux_66' model 'mux'
	mux_66 <=
		(repeat(32, sig_258) and sig_322) or
		(repeat(32, sig_271) and sig_289) or
		(repeat(32, sig_251) and augh_main_max_iter);

	-- Behaviour of component 'mux_67' model 'mux'
	mux_67 <=
		(sig_273 and '1') or
		(sig_270 and not_122) or
		(sig_250 and cp_en);

	-- Behaviour of component 'mux_68' model 'mux'
	mux_68 <=
		(repeat(32, sig_237) and stdin_data) or
		(repeat(32, sig_251) and cp_din(63 downto 32));

	-- Behaviour of component 'mux_69' model 'mux'
	mux_69 <=
		(sig_238 and not_122) or
		(sig_250 and cp_en);

	-- Behaviour of component 'mux_70' model 'mux'
	mux_70 <=
		(repeat(32, sig_240) and stdin_data) or
		(repeat(32, sig_251) and cp_din(31 downto 0));

	-- Behaviour of component 'mux_71' model 'mux'
	mux_71 <=
		(sig_239 and not_122) or
		(sig_250 and cp_en);

	-- Behaviour of component 'mux_72' model 'mux'
	mux_72 <=
		(repeat(8, sig_259) and cp_din(7 downto 0)) or
		(repeat(8, sig_267) and augh_main_i(7 downto 0));

	-- Behaviour of component 'mux_73' model 'mux'
	mux_73 <=
		(sig_259 and psc_loop_reg_3(0)) or
		(sig_247 and augh_main_i(3)) or
		(sig_245 and sig_298(3));

	-- Behaviour of component 'mux_74' model 'mux'
	mux_74 <=
		(sig_257 and and_116) or
		(sig_246 and sig_299) or
		(sig_244 and sig_222) or
		(sig_266 and and_190);

	-- Behaviour of component 'mux_75' model 'mux'
	mux_75 <=
		(sig_259 and psc_loop_reg_3(0)) or
		(sig_258 and sig_297(3)) or
		(sig_271 and augh_main_i(3));

	-- Behaviour of component 'mux_76' model 'mux'
	mux_76 <=
		(repeat(8, sig_259) and cp_din(15 downto 8)) or
		(repeat(8, sig_267) and augh_main_i(7 downto 0));

	-- Behaviour of component 'mux_77' model 'mux'
	mux_77 <=
		(sig_259 and psc_loop_reg_3(0)) or
		(sig_247 and augh_main_i(3)) or
		(sig_245 and sig_298(3));

	-- Behaviour of component 'mux_78' model 'mux'
	mux_78 <=
		(sig_257 and and_116) or
		(sig_246 and sig_226) or
		(sig_244 and sig_312) or
		(sig_266 and and_192);

	-- Behaviour of component 'mux_79' model 'mux'
	mux_79 <=
		(sig_259 and psc_loop_reg_3(0)) or
		(sig_258 and sig_297(3)) or
		(sig_271 and augh_main_i(3));

	-- Behaviour of component 'mux_80' model 'mux'
	mux_80 <=
		(repeat(8, sig_259) and cp_din(23 downto 16)) or
		(repeat(8, sig_267) and augh_main_i(7 downto 0)) or
		(repeat(8, sig_256) and "00001100");

	-- Behaviour of component 'mux_20' model 'mux'
	mux_20 <=
		(sig_250 and cp_en);

	-- Behaviour of component 'mux_22' model 'mux'
	mux_22 <=
		(sig_274 and sig_236) or
		(sig_254 and sig_281);

	-- Behaviour of component 'and_163' model 'and'
	and_163 <=
		sig_324 and
		repeat(8, sig_233);

	-- Behaviour of component 'and_164' model 'and'
	and_164 <=
		sig_325 and
		repeat(8, sig_232);

	-- Behaviour of component 'and_165' model 'and'
	and_165 <=
		sig_326 and
		repeat(8, sig_231);

	-- Behaviour of component 'and_166' model 'and'
	and_166 <=
		sig_327 and
		repeat(8, sig_230);

	-- Behaviour of component 'and_167' model 'and'
	and_167 <=
		sig_328 and
		repeat(8, sig_229);

	-- Behaviour of component 'and_168' model 'and'
	and_168 <=
		sig_329 and
		repeat(8, sig_228);

	-- Behaviour of component 'and_169' model 'and'
	and_169 <=
		sig_330 and
		repeat(8, sig_227);

	-- Behaviour of component 'and_182' model 'and'
	and_182 <=
		sig_336 and
		repeat(8, sig_321);

	-- Behaviour of component 'and_184' model 'and'
	and_184 <=
		sig_337 and
		repeat(8, sig_320);

	-- Behaviour of component 'and_186' model 'and'
	and_186 <=
		sig_338 and
		repeat(8, sig_290);

	-- Behaviour of component 'and_192' model 'and'
	and_192 <=
		not_122 and
		sig_319;

	-- Behaviour of component 'and_194' model 'and'
	and_194 <=
		not_122 and
		sig_318;

	-- Behaviour of component 'and_196' model 'and'
	and_196 <=
		not_122 and
		sig_225;

	-- Behaviour of component 'mux_81' model 'mux'
	mux_81 <=
		(sig_259 and psc_loop_reg_3(0)) or
		(sig_247 and augh_main_i(3)) or
		(sig_245 and sig_298(3));

	-- Behaviour of component 'mux_82' model 'mux'
	mux_82 <=
		(sig_257 and and_116) or
		(sig_246 and sig_315) or
		(sig_244 and sig_311) or
		(sig_266 and and_194) or
		(sig_255 and '1');

	-- Behaviour of component 'mux_83' model 'mux'
	mux_83 <=
		(sig_259 and psc_loop_reg_3(0)) or
		(sig_258 and sig_297(3)) or
		(sig_271 and augh_main_i(3));

	-- Behaviour of component 'mux_84' model 'mux'
	mux_84 <=
		(repeat(8, sig_259) and cp_din(31 downto 24)) or
		(repeat(8, sig_267) and augh_main_i(7 downto 0));

	-- Behaviour of component 'mux_85' model 'mux'
	mux_85 <=
		(sig_259 and psc_loop_reg_3(0)) or
		(sig_247 and augh_main_i(3)) or
		(sig_245 and sig_298(3));

	-- Behaviour of component 'mux_86' model 'mux'
	mux_86 <=
		(sig_257 and and_116) or
		(sig_246 and sig_225) or
		(sig_244 and sig_310) or
		(sig_266 and and_196);

	-- Behaviour of component 'mux_87' model 'mux'
	mux_87 <=
		(sig_259 and psc_loop_reg_3(0)) or
		(sig_258 and sig_297(3)) or
		(sig_271 and augh_main_i(3));

	-- Behaviour of component 'mux_88' model 'mux'
	mux_88 <=
		(repeat(8, sig_259) and cp_din(39 downto 32)) or
		(repeat(8, sig_267) and augh_main_i(7 downto 0));

	-- Behaviour of component 'mux_89' model 'mux'
	mux_89 <=
		(sig_259 and psc_loop_reg_3(0)) or
		(sig_247 and augh_main_i(3)) or
		(sig_245 and sig_298(3));

	-- Behaviour of component 'mux_90' model 'mux'
	mux_90 <=
		(sig_257 and and_116) or
		(sig_246 and sig_314) or
		(sig_244 and sig_309) or
		(sig_266 and and_197);

	-- Behaviour of component 'mux_91' model 'mux'
	mux_91 <=
		(sig_259 and psc_loop_reg_3(0)) or
		(sig_258 and sig_297(3)) or
		(sig_271 and augh_main_i(3));

	-- Behaviour of component 'mux_92' model 'mux'
	mux_92 <=
		(repeat(8, sig_259) and cp_din(47 downto 40)) or
		(repeat(8, sig_267) and augh_main_i(7 downto 0));

	-- Behaviour of component 'mux_93' model 'mux'
	mux_93 <=
		(sig_259 and psc_loop_reg_3(0)) or
		(sig_247 and augh_main_i(3)) or
		(sig_245 and sig_298(3));

	-- Behaviour of component 'mux_94' model 'mux'
	mux_94 <=
		(sig_257 and and_116) or
		(sig_246 and sig_224) or
		(sig_244 and sig_308) or
		(sig_266 and and_199);

	-- Behaviour of component 'mux_95' model 'mux'
	mux_95 <=
		(sig_259 and psc_loop_reg_3(0)) or
		(sig_258 and sig_297(3)) or
		(sig_271 and augh_main_i(3));

	-- Behaviour of component 'mux_96' model 'mux'
	mux_96 <=
		(repeat(8, sig_259) and cp_din(55 downto 48)) or
		(repeat(8, sig_267) and augh_main_i(7 downto 0));

	-- Behaviour of component 'mux_97' model 'mux'
	mux_97 <=
		(sig_259 and psc_loop_reg_3(0)) or
		(sig_247 and augh_main_i(3)) or
		(sig_245 and sig_298(3));

	-- Behaviour of component 'mux_98' model 'mux'
	mux_98 <=
		(sig_257 and and_116) or
		(sig_246 and sig_313) or
		(sig_244 and sig_307) or
		(sig_266 and and_201);

	-- Behaviour of component 'mux_99' model 'mux'
	mux_99 <=
		(sig_259 and psc_loop_reg_3(0)) or
		(sig_258 and sig_297(3)) or
		(sig_271 and augh_main_i(3));

	-- Behaviour of component 'mux_100' model 'mux'
	mux_100 <=
		(repeat(8, sig_259) and cp_din(63 downto 56)) or
		(repeat(8, sig_267) and augh_main_i(7 downto 0));

	-- Behaviour of component 'mux_101' model 'mux'
	mux_101 <=
		(sig_259 and psc_loop_reg_3(0)) or
		(sig_247 and augh_main_i(3)) or
		(sig_245 and sig_298(3));

	-- Behaviour of component 'mux_102' model 'mux'
	mux_102 <=
		(sig_257 and and_116) or
		(sig_246 and sig_223) or
		(sig_244 and sig_306) or
		(sig_266 and and_203);

	-- Behaviour of component 'mux_103' model 'mux'
	mux_103 <=
		(sig_259 and psc_loop_reg_3(0)) or
		(sig_258 and sig_297(3)) or
		(sig_271 and augh_main_i(3));

	-- Behaviour of component 'mux_106' model 'mux'
	mux_106 <=
		(sig_239 and sig_301) or
		(sig_238 and sig_316);

	-- Behaviour of component 'mux_108' model 'mux'
	mux_108 <=
		(sig_241 and sig_302);

	-- Behaviour of component 'mux_109' model 'mux'
	mux_109 <=
		(repeat(64, sig_263) and (sig_330 & sig_329 & sig_328 & sig_327 & sig_326 & sig_325 & sig_324 & sig_323)) or
		(repeat(64, sig_259) and (sig_338 & sig_337 & sig_336 & sig_335 & sig_334 & sig_333 & sig_332 & sig_331)) or
		(repeat(64, sig_251) and (psc_stuff_reg_11 & cp_id_reg_4));

	-- Behaviour of component 'mux_110' model 'mux'
	mux_110 <=
		(sig_265 and '1') or
		(sig_263 and sig_305) or
		(sig_259 and sig_317) or
		(sig_251 and sig_280);

	-- Behaviour of component 'and_116' model 'and'
	and_116 <=
		cp_en and
		cp_rest;

	-- Behaviour of component 'not_122' model 'not'
	not_122 <= not (
		cp_en
	);

	-- Behaviour of component 'or_143' model 'or'
	or_143 <=
		and_144 or
		and_155 or
		and_157 or
		and_159 or
		and_147 or
		and_149 or
		and_151 or
		and_153;

	-- Behaviour of component 'and_147' model 'and'
	and_147 <=
		sig_332 and
		repeat(8, sig_233);

	-- Behaviour of component 'and_149' model 'and'
	and_149 <=
		sig_333 and
		repeat(8, sig_232);

	-- Behaviour of component 'and_151' model 'and'
	and_151 <=
		sig_334 and
		repeat(8, sig_231);

	-- Behaviour of component 'and_153' model 'and'
	and_153 <=
		sig_335 and
		repeat(8, sig_230);

	-- Behaviour of component 'and_155' model 'and'
	and_155 <=
		sig_336 and
		repeat(8, sig_229);

	-- Behaviour of component 'and_157' model 'and'
	and_157 <=
		sig_337 and
		repeat(8, sig_228);

	-- Behaviour of component 'and_159' model 'and'
	and_159 <=
		sig_338 and
		repeat(8, sig_227);

	-- Behaviour of component 'or_161' model 'or'
	or_161 <=
		and_162 or
		and_167 or
		and_168 or
		and_169 or
		and_163 or
		and_164 or
		and_165 or
		and_166;

	-- Behaviour of component 'and_162' model 'and'
	and_162 <=
		sig_323 and
		repeat(8, sig_296);

	-- Behaviour of component 'or_172' model 'or'
	or_172 <=
		and_173 or
		and_182 or
		and_184 or
		and_186 or
		and_175 or
		and_177 or
		and_179 or
		and_181;

	-- Behaviour of component 'and_173' model 'and'
	and_173 <=
		sig_331 and
		repeat(8, sig_294);

	-- Behaviour of component 'and_175' model 'and'
	and_175 <=
		sig_332 and
		repeat(8, sig_293);

	-- Behaviour of component 'and_177' model 'and'
	and_177 <=
		sig_333 and
		repeat(8, sig_292);

	-- Behaviour of component 'and_179' model 'and'
	and_179 <=
		sig_334 and
		repeat(8, sig_291);

	-- Behaviour of component 'and_181' model 'and'
	and_181 <=
		sig_335 and
		repeat(8, sig_314);

	-- Behaviour of component 'and_190' model 'and'
	and_190 <=
		not_122 and
		sig_287;

	-- Behaviour of component 'and_197' model 'and'
	and_197 <=
		not_122 and
		sig_286;

	-- Behaviour of component 'and_199' model 'and'
	and_199 <=
		not_122 and
		sig_285;

	-- Behaviour of component 'and_201' model 'and'
	and_201 <=
		not_122 and
		sig_284;

	-- Behaviour of component 'and_203' model 'and'
	and_203 <=
		not_122 and
		sig_283;

	-- Behaviour of component 'or_205' model 'or'
	or_205 <=
		and_206 or
		and_211 or
		and_212 or
		and_213 or
		and_207 or
		and_208 or
		and_209 or
		and_210;

	-- Behaviour of component 'and_206' model 'and'
	and_206 <=
		sig_323 and
		repeat(8, sig_287);

	-- Behaviour of component 'and_207' model 'and'
	and_207 <=
		sig_324 and
		repeat(8, sig_319);

	-- Behaviour of component 'and_208' model 'and'
	and_208 <=
		sig_325 and
		repeat(8, sig_318);

	-- Behaviour of component 'and_209' model 'and'
	and_209 <=
		sig_326 and
		repeat(8, sig_225);

	-- Behaviour of component 'and_210' model 'and'
	and_210 <=
		sig_327 and
		repeat(8, sig_286);

	-- Behaviour of component 'and_211' model 'and'
	and_211 <=
		sig_328 and
		repeat(8, sig_285);

	-- Behaviour of component 'and_212' model 'and'
	and_212 <=
		sig_329 and
		repeat(8, sig_284);

	-- Behaviour of component 'and_213' model 'and'
	and_213 <=
		sig_330 and
		repeat(8, sig_283);

	-- Behaviour of component 'and_144' model 'and'
	and_144 <=
		sig_331 and
		repeat(8, sig_296);

	-- Behaviour of all components of model 'reg'
	-- Registers with clock = sig_clock and reset = sig_reset active '1'
	process(sig_clock, sig_reset)
	begin
		if sig_reset = '1' then
			psc_stuff_reg_11 <= "0000000000000000000000000000000000000000000000000000000000000";
		else
			if rising_edge(sig_clock) then
				if mux_20 = '1' then
					psc_stuff_reg_11 <= augh_main_i & augh_main_result(31 downto 3);
				end if;
			end if;
		end if;
	end process;
	-- Registers with clock = sig_clock and no reset
	process(sig_clock)
	begin
		if rising_edge(sig_clock) then
			if mux_27 = '1' then
				cp_id_reg_stable_5 <= mux_26;
			end if;
			if mux_29 = '1' then
				cp_id_reg_4 <= mux_28;
			end if;
			if mux_31 = '1' then
				psc_loop_reg_3 <= mux_30;
			end if;
			if mux_65 = '1' then
				augh_main_i <= mux_64;
			end if;
			if mux_67 = '1' then
				augh_main_result <= mux_66;
			end if;
			if mux_69 = '1' then
				augh_main_std_addition <= mux_68;
			end if;
			if mux_71 = '1' then
				augh_main_max_iter <= mux_70;
			end if;
		end if;
	end process;

	-- Remaining signal assignments
	-- Those who are not assigned by component instantiation

	sig_clock <= clock;
	sig_reset <= reset;
	sig_start <= start;
	test_cp_4_6 <= mux_25;
	test_cp_3_7 <= mux_24;
	test_cp_0_8 <= mux_23;
	test_cp_1_9 <= mux_22;
	memextrct_loop_sig_13 <= sig_303;
	memextrct_loop_sig_14 <= sig_303;
	sig_339 <= repeat(24, or_172(7)) & or_172;
	sig_340 <= or_161(7) & or_161;
	sig_341 <= or_143(7) & or_143;
	sig_342 <= repeat(23, sig_295(8)) & sig_295;

	-- Remaining top-level ports assignments
	-- Those who are not assigned by component instantiation

	stdin_rdy <= mux_106;
	stdout_data <= augh_main_result;
	stdout_rdy <= mux_108;
	cp_dout <= mux_109;
	cp_ok <= mux_110;

end architecture;
