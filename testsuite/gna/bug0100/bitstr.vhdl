entity bitstr is
  port (a : bit_vector (7 downto 0);
        o : out bit);
end bitstr;

architecture behav of bitstr is
begin
  process (a)
  begin
    if a = bx"00" then
     o <= '1';
    else
     o <= '0';
    end if;
  end process;
end behav;
