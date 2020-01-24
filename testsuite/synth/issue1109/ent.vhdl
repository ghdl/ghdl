library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ent is
	generic (
		INT : integer := -42;
		NAT : natural := 21
	);
	port (
		slv_a : in std_logic_vector(7 downto 0);
		slv_b : in std_logic_vector(7 downto 0);

		slv_eq : out std_logic;
		slv_ne : out std_logic;

		sl_a : in std_logic;
		sl_b : in std_logic;

		sl_lt : out std_logic;
		sl_le : out std_logic;
		sl_eq : out std_logic;
		sl_ne : out std_logic;
		sl_ge : out std_logic;
		sl_gt : out std_logic;

		uns_a : in unsigned(7 downto 0);
		uns_b : in unsigned(7 downto 0);

		uns_lt : out std_logic;
		uns_le : out std_logic;
		uns_eq : out std_logic;
		uns_ne : out std_logic;
		uns_ge : out std_logic;
		uns_gt : out std_logic;

		uns_int_lt : out std_logic;
		uns_int_le : out std_logic;
		uns_int_eq : out std_logic;
		uns_int_ne : out std_logic;
		uns_int_ge : out std_logic;
		uns_int_gt : out std_logic;

		int_uns_lt : out std_logic;
		int_uns_le : out std_logic;
		int_uns_eq : out std_logic;
		int_uns_ne : out std_logic;
		int_uns_ge : out std_logic;
		int_uns_gt : out std_logic;

		sgn_a : in signed(7 downto 0);
		sgn_b : in signed(7 downto 0);

		sgn_lt : out std_logic;
		sgn_le : out std_logic;
		sgn_eq : out std_logic;
		sgn_ne : out std_logic;
		sgn_ge : out std_logic;
		sgn_gt : out std_logic;

		sgn_nat_lt : out std_logic;
		sgn_nat_le : out std_logic;
		sgn_nat_eq : out std_logic;
		sgn_nat_ne : out std_logic;
		sgn_nat_ge : out std_logic;
		sgn_nat_gt : out std_logic;

		nat_sgn_lt : out std_logic;
		nat_sgn_le : out std_logic;
		nat_sgn_eq : out std_logic;
		nat_sgn_ne : out std_logic;
		nat_sgn_ge : out std_logic;
		nat_sgn_gt : out std_logic
	);
end;

architecture a of ent is
begin
	slv_eq <= slv_a ?=  slv_b;
	slv_ne <= slv_a ?/= slv_b;

	sl_lt <= sl_a ?<  sl_b;
	sl_le <= sl_a ?<= sl_b;
	sl_eq <= sl_a ?=  sl_b;
	sl_ne <= sl_a ?/= sl_b;
	sl_ge <= sl_a ?>= sl_b;
	sl_gt <= sl_a ?>  sl_b;

	uns_lt <= uns_a ?<  uns_b;
	uns_le <= uns_a ?<= uns_b;
	uns_eq <= uns_a ?=  uns_b;
	uns_ne <= uns_a ?/= uns_b;
	uns_ge <= uns_a ?>= uns_b;
	uns_gt <= uns_a ?>  uns_b;

	uns_int_lt <= uns_a ?<  NAT;
	uns_int_le <= uns_a ?<= NAT;
	uns_int_eq <= uns_a ?=  NAT;
	uns_int_ne <= uns_a ?/= NAT;
	uns_int_ge <= uns_a ?>= NAT;
	uns_int_gt <= uns_a ?>  NAT;

	int_uns_lt <= NAT ?<  uns_b;
	int_uns_le <= NAT ?<= uns_b;
	int_uns_eq <= NAT ?=  uns_b;
	int_uns_ne <= NAT ?/= uns_b;
	int_uns_ge <= NAT ?>= uns_b;
	int_uns_gt <= NAT ?>  uns_b;

	sgn_lt <= sgn_a ?<  sgn_b;
	sgn_le <= sgn_a ?<= sgn_b;
	sgn_eq <= sgn_a ?=  sgn_b;
	sgn_ne <= sgn_a ?/= sgn_b;
	sgn_ge <= sgn_a ?>= sgn_b;
	sgn_gt <= sgn_a ?>  sgn_b;

	sgn_nat_lt <= sgn_a ?<  INT;
	sgn_nat_le <= sgn_a ?<= INT;
	sgn_nat_eq <= sgn_a ?=  INT;
	sgn_nat_ne <= sgn_a ?/= INT;
	sgn_nat_ge <= sgn_a ?>= INT;
	sgn_nat_gt <= sgn_a ?>  INT;

	nat_sgn_lt <= INT ?<  sgn_b;
	nat_sgn_le <= INT ?<= sgn_b;
	nat_sgn_eq <= INT ?=  sgn_b;
	nat_sgn_ne <= INT ?/= sgn_b;
	nat_sgn_ge <= INT ?>= sgn_b;
	nat_sgn_gt <= INT ?>  sgn_b;
end;
