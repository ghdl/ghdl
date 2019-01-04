entity whide is
end entity;

architecture a of whide is
  signal a : boolean;
begin
  process(a)
    variable whide : boolean;
  begin
    case a is
      when false =>  report "FALSE";
      when true =>   report "TRUE";
      when others => report "others";
    end case;
  end process;
end architecture;

