------------------------------------------------------------------------------
--  Copyright (c) 2018 by Paul Scherrer Institute, Switzerland
--  All rights reserved.
--  Authors: Oliver Bruendler
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Libraries
------------------------------------------------------------------------------
library ieee ;
	use ieee.std_logic_1164.all;
	use ieee.numeric_std.all;

library work;
	use work.psi_common_array_pkg.all;

------------------------------------------------------------------------------
-- Package Header
------------------------------------------------------------------------------
package psi_common_math_pkg is

	function log2(arg : in natural) return natural;
	
	function log2ceil(arg : in natural) return natural;
	
	function log2ceil(arg : in real) return natural;	
	
	function isLog2(arg : in natural) return boolean;
	
	function max(	a : in integer;
					b : in integer) return integer;
					
	function min(	a : in integer;
					b : in integer) return integer;
			
	-- choose t if s=true else f
	function choose(	s : in 	boolean;
						t : in 	std_logic;
						f : in 	std_logic) return std_logic;
						
	function choose(	s : in 	boolean;
						t : in 	std_logic_vector;
						f : in 	std_logic_vector) return std_logic_vector;		

	function choose(	s : in 	boolean;
						t : in 	integer;
						f : in 	integer) return integer;	

	function choose(	s : in 	boolean;
						t : in 	string;
						f : in 	string) return string;
						
	function choose(	s : in 	boolean;
						t : in 	real;
						f : in 	real) return real;	
						
	function choose(	s : in 	boolean;
						t : in 	unsigned;
						f : in 	unsigned) return unsigned;	
						
	-- count occurence of a value inside an array
	function count(	a : in t_ainteger;
					v : in integer) return integer;
					
	function count(	a : in t_abool;
					v : in boolean) return integer;	

	function count(	a : in std_logic_vector;
					v : in std_logic) return integer;	
  
end psi_common_math_pkg;	 

------------------------------------------------------------------------------
-- Package Body
------------------------------------------------------------------------------
package body psi_common_math_pkg is 
  
	-- *** Log2 integer ***
	function log2(arg : in natural) return natural is
		variable v : natural := arg;
		variable r : natural := 0;
	begin
		while v > 1 loop
			v := v/2;
			r := r+1;
		end loop;
		return r;
	end function;
	
	-- *** Log2Ceil integer ***
	function log2ceil(arg : in natural) return natural is
	begin
		if arg = 0 then
			return 0;
		end if;
		return log2(arg*2-1);
	end function;
	
	-- *** Log2Ceil real ***
	function log2ceil(arg : in real) return natural is
		variable v : real := arg;
		variable r : natural := 0;
	begin
		while v > 1.0 loop
			v := v/2.0;
			r := r+1;
		end loop;
		return r;
	end function;	
	
	-- *** isLog2 ***
	function isLog2(arg : in natural) return boolean is
	begin
		if log2(arg) = log2ceil(arg) then
			return true;
		else
			return false;
		end if;
	end function;
	
	-- *** Max ***
	function max(	a : in integer;
					b : in integer) return integer is
	begin
		if a > b then 
			return a;
		else
			return b;
		end if;
	end function;
				
	-- *** Min ***			
	function min(	a : in integer;
					b : in integer) return integer is
	begin
		if a > b then 
			return b;
		else
			return a;
		end if;
	end function;	
	
	-- *** Choose (std_logic) ***	
	function choose(	s : in 	boolean;
						t : in 	std_logic;
						f : in 	std_logic) return std_logic is
	begin
		if s then
			return t;
		else
			return f;
		end if;
	end function;
	
	-- *** Choose (std_logic_vector) ***	
	function choose(	s : in 	boolean;
						t : in 	std_logic_vector;
						f : in 	std_logic_vector) return std_logic_vector is
	begin
		if s then
			return t;
		else
			return f;
		end if;
	end function;	
	
	-- *** Choose (integer) ***	
	function choose(	s : in 	boolean;
						t : in 	integer;
						f : in 	integer) return integer is
	begin
		if s then
			return t;
		else
			return f;
		end if;
	end function;	

	-- *** Choose (string) ***	
	function choose(	s : in 	boolean;
						t : in 	string;
						f : in 	string) return string is
	begin
		if s then
			return t;
		else
			return f;
		end if;
	end function;	

	-- *** Choose (real) ***	
	function choose(	s : in 	boolean;
						t : in 	real;
						f : in 	real) return real is
	begin
		if s then
			return t;
		else
			return f;
		end if;
	end function;
	
	-- *** Choose (unsigned) ***	
	function choose(	s : in 	boolean;
						t : in 	unsigned;
						f : in 	unsigned) return unsigned is
	begin
		if s then
			return t;
		else
			return f;
		end if;
	end function;

	-- *** count (integer) ***	
	function count(	a : in t_ainteger;
					v : in integer) return integer is
		variable cnt_v : integer := 0;
	begin
		for idx in a'low to a'high loop
			if a(idx) = v then
				cnt_v := cnt_v+1;
			end if;
		end loop;
		return cnt_v;
	end function;
	
	-- *** count (bool) ***		
	function count(	a : in t_abool;
					v : in boolean) return integer is
		variable cnt_v : integer := 0;
	begin
		for idx in a'low to a'high loop
			if a(idx) = v then
				cnt_v := cnt_v+1;
			end if;
		end loop;
		return cnt_v;
	end function;

	-- *** count (std_logic) ***	
	function count(	a : in std_logic_vector;
					v : in std_logic) return integer is
		variable cnt_v : integer := 0;
	begin
		for idx in a'low to a'high loop
			if a(idx) = v then
				cnt_v := cnt_v+1;
			end if;
		end loop;
		return cnt_v;
	end function;
	
end psi_common_math_pkg;





