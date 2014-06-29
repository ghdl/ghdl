entity repro is
    generic (
	BUS_WIDTH : integer := 8;
	ARRAY_WIDTH : integer := 2);
end entity repro;

architecture behavioural of repro is

    type test_array_type is array (ARRAY_WIDTH-1 downto 0) of
      bit_vector (BUS_WIDTH-1 downto 0);
    signal s : test_array_type := (others => (others => '0'));

begin

    failing_process : process
    begin
      assert s'left = 1;
      assert s'right = 0;
      wait;
    end process failing_process;
    
end architecture behavioural;
