entity enum01 is
end;

architecture arch of enum01 is
  type t_state is (s0, s1, s2);
  signal s : t_state;
begin
  process
  begin
    case s is
      when s0 =>
        null;
    end case;
    wait;
  end process;
end arch;
