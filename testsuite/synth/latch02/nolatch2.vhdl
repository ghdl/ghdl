entity nolatch2 is
  port (sel : bit;
        rst : bit;
        clk : bit;
        r : out bit);
end;

architecture arch of nolatch2 is
  type t_enum is (S0, S3);
  signal state, nstate : t_enum;
begin
  process (all)
    variable a : bit;
  begin
    nstate <= state;
    r <= '0';
    
    case state is
      when S0 =>
        if sel = '0' then
          nstate <= S3;
        end if;
      when S3 =>
        r <= '1';
        if sel = '1' then
          a := '1';
          r <= '0';
        end if;
        nstate <= S0;
    end case;
  end process;

  process (clk)
  begin
    if clk'event and clk = '1' then
      if rst = '1' then
        state <= S0;
      else
        state <= nstate;
      end if;
    end if;
  end process;
end;

