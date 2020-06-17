entity repro3 is
  generic (hi : natural := 1);
end repro3;

architecture behav of repro3 is
   signal left : bit_vector(hi downto 0);
begin
    process
    begin
      for i in 1 to 2 loop
        left(i) <= '1';
      end loop;
      wait;
    end process;
end;
