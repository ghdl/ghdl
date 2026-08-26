library ieee;
context ieee.ieee_std_context;

entity ripple_carry_adder is
    generic (bit_count: positive);
	port (
		sum : out std_logic_vector(bit_count - 1 downto 0);
		c_out : out std_logic
	);
end entity;

architecture structural of ripple_carry_adder is
begin
end architecture;
