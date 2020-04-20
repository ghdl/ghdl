package pkg is
	type c_int_prot is protected
		impure function get return integer;
		--------------------------------------------
		impure function c_getInt return integer;
		attribute foreign of c_getInt : function is "VHPIDIRECT getInt";
		--------------------------------------------
	end protected c_int_prot;
	--------------------------------------------
	-- impure function c_getInt return integer;
	-- attribute foreign of c_getInt : function is "VHPIDIRECT getInt";
	--------------------------------------------

	shared variable c_int : c_int_prot;
end package;

package body pkg is
	type c_int_prot is protected body
		variable hidden_c_int : integer := c_getInt;

		impure function get return integer is
		begin
			return hidden_c_int;
		end function;

		--------------------------------------------
		impure function c_getInt return integer is
		begin
			assert false report "c_getInt VHPIDIRECT" severity failure;
		end function;
		--------------------------------------------
	end protected body c_int_prot;
	--------------------------------------------
	-- impure function c_getInt return integer is
	-- begin
	-- 	assert false report "c_getInt VHPIDIRECT" severity failure;
	-- end function;
	--------------------------------------------
end package body;
