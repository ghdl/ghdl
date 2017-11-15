entity repro is
end entity;

architecture A of repro is
	signal S1           : bit := '0';
	alias S1_delayed   : bit is S1'delayed(100 ns);
begin
	S1 <= '1' after 10 ns, '0' after 20 ns;

        process (S1) is
        begin
          assert false report "S1 = " & bit'image(S1) severity note;
        end process;

        process (S1_delayed) is
        begin
          assert false report "S1'delayed = " & bit'image(S1_delayed)
            severity note;
        end process;
end architecture;
