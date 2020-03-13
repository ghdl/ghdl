library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ent is
	port (
		sgn : signed(7 downto 0) := x"f8";
		uns : unsigned(7 downto 0) := x"07";
		nat : natural := 15;
		int : integer := -3;

		mul_int_int : out integer;
		mul_uns_uns : out unsigned(15 downto 0);
		mul_uns_nat : out unsigned(15 downto 0);
		mul_nat_uns : out unsigned(15 downto 0);
		mul_sgn_sgn : out signed(15 downto 0);
		mul_sgn_int : out signed(15 downto 0);
		mul_int_sgn : out signed(15 downto 0);

		div_int_int : out integer;
		div_uns_uns : out unsigned(7 downto 0);
		div_uns_nat : out unsigned(7 downto 0);
		div_nat_uns : out unsigned(7 downto 0);
		div_sgn_sgn : out signed(7 downto 0);
		div_sgn_int : out signed(7 downto 0);
		div_int_sgn : out signed(7 downto 0);

		rem_int_int : out integer;
		rem_uns_uns : out unsigned(7 downto 0);
		rem_uns_nat : out unsigned(7 downto 0);
		rem_nat_uns : out unsigned(7 downto 0);
		rem_sgn_sgn : out signed(7 downto 0);
		rem_sgn_int : out signed(7 downto 0);
		rem_int_sgn : out signed(7 downto 0);

		mod_int_int : out integer;
		mod_uns_uns : out unsigned(7 downto 0);
		mod_uns_nat : out unsigned(7 downto 0);
		mod_nat_uns : out unsigned(7 downto 0);
		mod_sgn_sgn : out signed(7 downto 0);
		mod_sgn_int : out signed(7 downto 0);
		mod_int_sgn : out signed(7 downto 0)
	);
end;

architecture a of ent is
begin
	mul_int_int <= int * int;
	mul_uns_uns <= uns * uns;
	mul_uns_nat <= uns * nat;
	mul_nat_uns <= nat * uns;
	mul_sgn_sgn <= sgn * sgn;
	mul_sgn_int <= sgn * int;
	mul_int_sgn <= int * sgn;

	div_int_int <= int / int;
	div_uns_uns <= uns / uns;
	div_uns_nat <= uns / nat;
	div_nat_uns <= nat / uns;
	div_sgn_sgn <= sgn / sgn;
	div_sgn_int <= sgn / int;
	div_int_sgn <= int / sgn;

	rem_int_int <= int rem int;
	rem_uns_uns <= uns rem uns;
	rem_uns_nat <= uns rem nat;
	rem_nat_uns <= nat rem uns;
	rem_sgn_sgn <= sgn rem sgn;
	rem_sgn_int <= sgn rem int;
	rem_int_sgn <= int rem sgn;

	mod_int_int <= int mod int;
	mod_uns_uns <= uns mod uns;
	mod_uns_nat <= uns mod nat;
	mod_nat_uns <= nat mod uns;
	mod_sgn_sgn <= sgn mod sgn;
	mod_sgn_int <= sgn mod int;
	mod_int_sgn <= int mod sgn;
end;
