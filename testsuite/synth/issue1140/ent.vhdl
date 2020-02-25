library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ent is
	generic (NUM_CHANNELS : natural := 4);
	port (
		iar   : in  unsigned(15 downto 0);
		ipend : in  unsigned(NUM_CHANNELS-1 downto 0);
		irq   : out unsigned(3 downto 0);
		clk   : in std_logic
	);
end;

architecture a of ent is
	type irq_t is
		array (integer range 0 to 4-1) of unsigned(NUM_CHANNELS-1 downto 0);

	signal imap : irq_t;

	function or_reduce (v: in unsigned) return std_logic is
		variable rv : std_logic := '0';
	begin
		for i in v'range loop
			rv := rv or v(i);
		end loop;
		return rv;
	end function;


begin

gen_irqmap:
  for i in 0 to 4-1 generate
	irq(i) <= or_reduce(imap(i));
  end generate;

irq_map:
	process (clk)
		variable itmp : irq_t := (others => (others => '0'));
	begin
		-- IRQ channel assignment:
		if rising_edge(clk) then
			for i in 0 to NUM_CHANNELS-1 loop
				itmp(to_integer(iar(i*2+1 downto i*2)))(i) := ipend(i);
			end loop;
			imap <= itmp;
		end if;
	end process;
end;

