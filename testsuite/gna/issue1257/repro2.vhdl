entity repro2 is
end repro2;

architecture behav of repro2 is
   signal left : bit_vector(1 downto 0);
begin
    process
    begin
      for i in 1 to 2 loop
        left(i) <= '1';
      end loop;
      wait;
    end process;
end;
