--
-- indexing testcase "B" derived from gna bug16782
--
--  ghdl-0.31-mcode on win32 : indexing off the end of a bit_vector results in an overflow exception message
--

entity index_range_test_B is 
end entity;

architecture arch of index_range_test_B is

 signal a    : bit_vector(15 downto 0);
 signal tmp  : bit;
 signal i    : integer;

 constant DATAPATH : natural := a'length;

begin

 i   <= DATAPATH + 3;

 tmp <= a(i);

end architecture;