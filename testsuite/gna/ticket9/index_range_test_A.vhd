--
-- indexing testcase "A" derived from gna bug16782
--
--  ghdl-0.31-mcode on win32 : indexing off the end of an unconstrained port results in an unhandled exception
--
entity comp2 is

  port(a :in bit_vector);

end entity;

architecture arch of comp2 is

  constant DATAPATH : natural := a'length;
  signal   tmp      : bit;

begin

  tmp <= a(DATAPATH+3);

end architecture;


entity index_range_test_A is end entity;

architecture arch of index_range_test_A is

  constant DATAPATH :natural := 16;
  signal a :bit_vector(DATAPATH-1 downto 0);

begin

  i_comp: entity work.comp2 port map(a);

end architecture;