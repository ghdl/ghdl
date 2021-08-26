library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

entity entity_1 is
	generic (
		FREQ : real     := 100.0;
		BITS : positive := 8.5 ns;
		type Typ
	);
	port (
	  Clock: in  std_logic := 5 ns;
	  Reset: in  std_logic := '0';
	  D:     inout bit_vector(clock'range);
	  Q:     out std_logic_vector(BITS'left - 1 downto Re.set)
	);

	constant fire : boolean := True;
begin
	wood <= fire;
end entity entity_1;

architecture behav of entity_1 is
	constant MAX : positive := -25;

	signal rst : std_logic := foo('U');
	signal vec : bit_vector(pack(3 to 2).signaal'range'value);
	signal copy : input'subtype;

	type newInt is range -4 to 3;
	type newFp is range 4.3 downto -3.9;
	type arr is array(natural range <>, enum range <>) of integer(3 downto 0);
	type rec is record
		elem1 : bit;
		elem2 : boolean;
		elem3 : integer_vector(3 downto 0);
		elem4 : natural range 7 to 8;
	end record;
	type enum is (e1, e2, e3);
	type acc is access bar;
	type fil is file of string;
	subtype uint8 is integer range 0 to 255;

	file f : text;

	function func (a : integer; b : boolean) return bit is
	begin

	end function;

  shared variable pt_var : lib.pack.prot;

	procedure proc(spam : egg) is
	begin

	end procedure;

	type prot is protected
		function meth(a : int) return bit;
	end protected;

	type prot is protected body
		variable var : positive;
		constant const : boolean;

		function meth(a : int) return bit is
		begin

		end function;
	end protected body;

	package pack_inst is new generic_pack
		generic map (
			BITS => 32
		);

	attribute att : boolean;

	alias bar is boolean;

	disconnect address_bus : resolved_word after 3 ns;
	disconnect others : resolved_word after 2 ns;

--	default clock is rising_edge(clk);
	package inner_pack is
	end package;
begin
	proc: process(Clock)
	begin
		if rising_edge(Clock) then
			if Reset = '1' then
				Q <= (others => '0');
			elsif Load = '1' then
				Q <= D after 10 ns;
			else
				Q <= std_logic_vector(unsigned(Q) + 1);
				counter.increment(1);
			end if;
		end if;

		for i in 7 downto 0 loop
			loop
				while true loop
					next;
					next when true;
				end loop;
				exit;
				exit when true;
			end loop;
			return;
		end loop;

		case foo_bar is
			when 0 =>
				report "hello" & " " & "world";
			when 1 | 2 =>
				report "vhdl" severity note;
			when 3 to 4 =>
				assert true nor false report "nothing";
			when 5 to 6 | 8 to 9 =>
				assert true nor false report "nothing" severity warning or error;
			when others =>
		end case;

		wait;
		wait on a, b;
		wait until rising_edge(clock);
		wait on clock until rising_edge(clock);
		wait for 10 ns;
		wait on c for 50 ns;
		wait until rising_edge(clock) for 100 ns;
		wait on sel until rising_edge(clock) for 100 ns;
	end process;

	a <= b;

	assert false;
	assert false report "some error";
	assert false severity warning;
	assert false report "some note" severity note;

	inst1: entity work.counter1(rtl)
		generic map (
			BITS1 => 8
		)
		port map (
			clk1 => Clock
		);

	inst2: component counter2
		generic map (
			BITS2 => 8,
			value2
		)
		port map (
			clk2 => Clock,
			enable2
		);

	inst3: configuration counter3
		generic map (
			BITS3 => 8
		)
		port map (
			clk3 => Clock,
			control(0) => battery and emergency
		);

	blk: block
	begin
		inst4: entity work.counter4(rtl)
			port map (
				clk => Clock,
				value => open
			);
	end block;

	genIf: if True generate
		constant G0 : boolean := False;
	begin
		inst: component IfDummy;
	elsif False generate
		constant G1 : boolean := False;
	begin
		inst: component ElsifDummy;
	else generate
		constant G2 : boolean := False;
	begin
		inst: component ElseDummy;
	end generate;

	genFor: for I in 0 to 3 generate
		constant G3 : boolean := False;
	begin
		inst: component ForDummy;
	end generate;

	genCase: case selector generate
		when 0 =>
				constant G4 : boolean := False;
			begin
				inst: component Case0Dummy;

		when 1 | 2 =>
				constant G5 : boolean := False;
			begin
				inst: component Case12Dummy;

		when 3 to 4 =>
				constant G6 : boolean := False;
			begin
				inst: component Case34Dummy;

		when 5 to 6 | 8 to 9 =>
				constant G7 : boolean := False;
			begin
				inst: component Case5689Dummy;
				process
				begin
					null;
					wait;
				end process;

		when others =>
				constant G8 : boolean := False;
			begin
				blkOthers: block
					constant G9 : boolean := False;
				begin
					ifOthers: if false generate
						constant G10 : boolean := False;
					begin
						inst: component OthersDummy;
					end generate;
				end block;
	end generate;

	call: CallDummy;
	called: CalledDummy(25);
	ende: std.env.stop;
end architecture behav;

context ctx is
	library osvvm;
	library axi4_lite, axi4_stream;
	use osvvm.alert.all;
	use osvvm.alert.alertid, osvvm.alert.priority;
end context;


context work.ctx;

package package_1 is
	generic (
		BITS : positive
	);

	use lib.pack.all;

  type cell;

	constant ghdl : float := (3, 5, 0 to 2 => 5, 3 => 4, name => 10, others => 10, 2.3);
	attribute fixed of ghdl, gtkwave [x, y] : constant is true;

	component comp is
		generic (
			BITS : positive := 2
		);
		port (
			clk : std
		);
	end component;

	constant Pointer_1 : List := new List(1 to 1);
	constant Pointer_2 : List := new List'(1 => 0);
	signal init : std_logic_vector(abs(mssb_idx(GEN)-GEN'right)-1 downto 0);
	constant fid : real := +val;
	constant ceq11 : std_logic := '1' ?= '1';
	type rt321 is range t3'reverse_range;
	type rt321 is range t3'reverse_range(1);
end package;

package body package_1 is
	constant ghdl : float := 1.5;

	type CAPACITY is range 0 to 1E5	units
		pF;
		nF = 1000 pF;
		uF = 1000 nF;
		mF = 1000 uF;
		F = 1000 mF;
	end units;
end package body;

vunit vu (component_1) {

}
