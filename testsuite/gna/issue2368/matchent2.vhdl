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
  constant K17 : unsigned(4 downto 0) := to_unsigned(17, 5);
  constant K21 : unsigned(4 downto 0) := to_unsigned(21, 5);
  constant K34 : unsigned(4 downto 0) := to_unsigned(34, 5);
begin
  tester : process (A, B)
  begin
    case A is
      when K17 =>
        XOUT <= A + 1;
      when K21 =>
        XOUT <= A + B;
      when K34 =>
        XOUT <= A - B;
      when others =>
        XOUT <= resize(A * B, 5);
    end case;
  end process;
end architecture;
