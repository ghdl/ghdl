entity repro is
end entity;

architecture A of repro is
	signal S1           : bit := '0';
	signal S2_transport : bit;
	signal S2_delayed   : bit;
begin
	S1 <= '1' after 10 ns, '0' after 20 ns;
	S2_transport <= transport S1 after 100 ns;
	S2_delayed   <= S1'delayed(100 ns);

        process (S1) is
        begin
          assert false report "S1 = " & bit'image(S1) severity note;
        end process;

        process (S2_delayed) is
        begin
          assert false report "S1'delayed = " & bit'image(S2_delayed) severity note;
        end process;
end architecture;
