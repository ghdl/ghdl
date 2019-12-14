library ieee;
	use ieee.std_logic_1164.all;
	use ieee.numeric_std.all;


entity test_core is
	generic (
		CONFIG_NUM_GPIO : natural := 2;
		GP_OFFSET : natural := 16
	);
	port (
		gpio         : inout unsigned(CONFIG_NUM_GPIO*16-1 downto 0);
		-- Inter-module connection wires for pin muxing: {
		gpio_in      : out std_logic_vector(CONFIG_NUM_GPIO*16-1 downto 0);
		gpio_out     : in  std_logic_vector(CONFIG_NUM_GPIO*16-1 downto 0);
		gpio_dir     : in  std_logic_vector(CONFIG_NUM_GPIO*16-1 downto 0)

	);
end entity;

architecture behaviour of test_core is

	constant num_pins : natural := CONFIG_NUM_GPIO*16;

	-- constant GP_OFFSET : natural := 16;

	subtype PORT_A_RANGE is natural range 16-1 downto 0;
	subtype PORT_B_RANGE is integer range GP_OFFSET+15 downto GP_OFFSET;

	signal io_read : std_logic_vector(num_pins-1 downto 0);
	signal out_int : std_logic_vector(num_pins-1 downto 0);
	signal io_dir  : std_logic_vector(num_pins-1 downto 0);

	alias out_b_int_mux : std_logic_vector(16-1 downto 0) is
		out_int(31 downto 16);

	alias iodir_b_mux : std_logic_vector(16-1 downto 0) is
		io_dir(PORT_B_RANGE);

	alias gpio_b_dir : std_logic_vector(16-1 downto 0) is
		gpio_dir(PORT_B_RANGE);

	alias gpio_b_out : std_logic_vector(16-1 downto 0) is
		gpio_out(PORT_B_RANGE);

	alias io_b_read : std_logic_vector(16-1 downto 0) is
		io_read(PORT_B_RANGE);

begin

gpio_direction:
	for i in 0 to num_pins-1 generate
		gpio(i) <= out_int(i) when io_dir(i) = '1' else 'Z';
	end generate;

	io_read <= std_logic_vector(gpio);
	gpio_in <= io_read;

mux_port_b:
	process (gpio_b_out, gpio_b_dir)
	begin
		-- default:
		out_b_int_mux <= gpio_b_out;
		iodir_b_mux   <= gpio_b_dir;


	end process;
end architecture;
