library IEEE;
use     IEEE.std_logic_1164.all;
use     IEEE.numeric_std.all;

package my_pkg is
	type    T_SLVV              is array(NATURAL range <>) of STD_LOGIC_VECTOR;
	subtype T_SLVV_8            is T_SLVV(open)(7 downto 0);

	function to_slv(slvv : T_SLVV)                return std_logic_vector;

	function to_slvv(slv : std_logic_vector; sub_element_length : natural) return T_SLVV;

	function to_slvv_8(slv : std_logic_vector) return T_SLVV_8;

end my_pkg;

package body my_pkg is



	function to_slv(slvv : T_SLVV) return std_logic_vector is
		constant first_len  : natural := slvv'length;
		constant second_len : natural := slvv(slvv'left)'length;
		variable slv        : std_logic_vector((first_len * second_len) - 1 downto 0);
	begin
		report "to_slv:: slvv'length=" & integer'image(slvv'length) severity note;
		report "to_slv:: first_len=" & integer'image(first_len) severity note;
		report "to_slv:: slvv(slvv'left)'length=" & integer'image(slvv(slvv'left)'length) severity note;
		report "to_slv:: second_len=" & integer'image(second_len) severity note;
		report "to_slv:: slv'length=" & integer'image(slv'length) severity note;

		for i in slvv'range loop
			slv(((i - slvv'low) * second_len) + second_len -1 downto ((i - slvv'low) * second_len))   := slvv(i);
		end loop;
		return slv;
	end function;

	function to_slvv(slv : std_logic_vector; sub_element_length : natural) return T_SLVV is
		variable Result   : T_SLVV((slv'length / sub_element_length) - 1 downto 0)(sub_element_length -1 downto 0);
	begin
		report "to_slvv: slv'length=" & integer'image(slv'length) & ")" severity note;
		report "to_slvv:: slv'left="  & integer'image(slv'left) & ", slv'right=" & integer'image(slv'right) severity note;
		-- report "to_slvv:: slv'high="  & integer'image(slv'high) & ", slv'low=" & integer'image(slv'low) severity note;

		for i in Result'range loop
			Result(i) := slv((i * sub_element_length) + sub_element_length -1 +slv'low downto (i * sub_element_length) +slv'low);
		end loop;
		return Result;
	end function;

	function to_slvv_8(slv : std_logic_vector) return T_SLVV_8 is
	begin
		return to_slvv(slv, 8);
	end function;
end package body;
