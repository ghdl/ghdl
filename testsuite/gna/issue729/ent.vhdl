entity e is
end entity;

architecture a of e is
  signal a : boolean;
begin
  process(a)
  begin
    case a is
      when false =>  report "FALSE";
      when true =>   report "TRUE";
      when others => report "others";
    end case;
  end process;
end architecture;

