library ieee;
use ieee.numeric_std.all;

entity MatchEnt is
  port (
    A : in unsigned(4 downto 0);
    B : in unsigned(4 downto 0);
    XOUT : out unsigned(4 downto 0)
  );
end entity;

architecture behavior of MatchEnt is
begin
  tester : process (A, B)
  begin
    case A is
      when to_unsigned(17, 5) =>
        XOUT <= A + 1;
      when to_unsigned(21, 5) =>
        XOUT <= A + B;
      when to_unsigned(34, 5) =>
        XOUT <= A - B;
      when others =>
        XOUT <= resize(A * B, 5);
    end case;
  end process;
end architecture;
