entity bug is
	port (
		clk : in bit
	);
end bug;

architecture struct of bug is

	type data_t is record
		value : natural;
	end record;

	type offset_array_t is array(natural range<>) of natural;
	type data_array_t is array(natural range<>) of data_t;

	type collection_t is record
		offset : offset_array_t;
		data   : data_array_t;
	end record;

	-- This results in an error
	function append(prefix : collection_t; data : data_array_t) return collection_t is
	begin
		return (offset => prefix.offset & prefix.data'length, 
		        data   => prefix.data   & data);
	end function;

	-- This works as a workaround
	function append2(prefix : collection_t; data : data_array_t) return collection_t is
		variable ret : collection_t(offset(0 to prefix.offset'length), data(0 to prefix.data'length+data'length-1));
	begin
		ret := (offset => prefix.offset & prefix.data'length, 
		        data   => prefix.data   & data);
		return ret;
	end function;

	--intentional null ranges for the initial value
	constant initial_collection : collection_t(offset(0 to -1), data(0 to -1)) := (offset => (others => 0), data => (others => (value => 0)));
	constant new_data : data_array_t(0 to 2) := (others => (value => 0));

	constant final_collection : collection_t := append(initial_collection, new_data);
begin
	
end architecture;
