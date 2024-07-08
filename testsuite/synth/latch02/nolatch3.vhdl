entity nolatch2 is
  port (sel : bit;
        rst : bit;
        clk : bit;
        r : out bit);
end;

architecture arch of nolatch2 is
  type t_enum is (S0, S1, S2, S3, S4);
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
          nstate <= S1;
        end if;
      when S1 =>
        if sel = '0' then
          nstate <= S2;
        end if;
      when S2 =>
        if sel = '1' then
          nstate <= S3;
        end if;
      when S3 =>
        r <= '1';
        if sel = '1' then
          a := '1';
          r <= '0';
        end if;
        nstate <= S4;
      when S4 =>
        if sel = '1' then
          nstate <= S2;
        else
          nstate <= S0;
        end if;
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

