library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ent is
	generic (
		INT : integer := -25;
		SL  : std_logic := '1'
	);
	port (
		a : in signed(7 downto 0);
		b : in signed(7 downto 0);

		const : out signed(7 downto 0);
		absolute1 : out unsigned(7 downto 0);
		absolute2 : out unsigned(7 downto 0);
		sum  : out signed(8 downto 0);
		diff : out signed(8 downto 0);
		inv_diff : out signed(8 downto 0);
		quarter : out signed(7 downto 0);

		int_sum  : out signed(8 downto 0);
		int_diff : out signed(8 downto 0);

		inv_int_sum  : out signed(8 downto 0);
		inv_int_diff : out signed(8 downto 0);

		sl_sum  : out signed(8 downto 0);
		sl_diff : out signed(8 downto 0);

		inv_sl_sum  : out signed(8 downto 0);
		inv_sl_diff : out signed(8 downto 0);

		lt : out boolean;
		le : out boolean;
		eq : out boolean;
		neq : out boolean;
		ge : out boolean;
		gt : out boolean;

		int_lt : out boolean;
		int_le : out boolean;
		int_eq : out boolean;
		int_neq : out boolean;
		int_ge : out boolean;
		int_gt : out boolean;

		inv_int_lt : out boolean;
		inv_int_le : out boolean;
		inv_int_eq : out boolean;
		inv_int_neq : out boolean;
		inv_int_ge : out boolean;
		inv_int_gt : out boolean
	);
end;

architecture a of ent is
	signal ra, rb : signed(8 downto 0);
begin
	ra <= resize(a, 9);
	rb <= resize(b, 9);

	const <= to_signed(INT, const'length);
	absolute1 <= to_unsigned(abs(INT), absolute1'length);
	absolute2 <= unsigned(abs(a));
	sum <= ra + rb;
	diff <= ra + (-rb);
	inv_diff <= rb - ra;
	quarter <= a / 4;

	int_sum <= ra + INT;
	int_diff <= ra - INT;

	inv_int_sum <= INT + ra;
	inv_int_diff <= INT - ra;

	sl_sum <= ra + SL;
	sl_diff <= ra - SL;

	inv_sl_sum <= SL + ra;
	inv_sl_diff <= SL - ra;

	lt  <= a < b;
	le  <= a <= b;
	eq  <= a = b;
	neq <= a /= b;
	ge  <= a >= b;
	gt  <= a > b;

	int_lt  <= a < INT;
	int_le  <= a <= INT;
	int_eq  <= a = INT;
	int_neq <= a /= INT;
	int_ge  <= a >= INT;
	int_gt  <= a > INT;

	inv_int_lt  <= INT < b;
	inv_int_le  <= INT <= b;
	inv_int_eq  <= INT = b;
	inv_int_neq <= INT /= b;
	inv_int_ge  <= INT >= b;
	inv_int_gt  <= INT > b;
end;
