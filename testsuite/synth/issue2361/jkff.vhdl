entity jkff is
  port (
    j_n, k_n, clk : in bit;
    q, q_n : inout bit);
end entity;

architecture behavioral of jkff is
begin
  flip_flop : process (j_n, k_n, clk) is
  begin
    case j_n & k_n & clk is
      when "010" | "011" => q <= '1';
      when "100" | "101" => q <= '0';
      when "001" => q <= q_n;
      when others => q <= unaffected;
    end case;
  end process;
  q_n <= not q;
end architecture;
