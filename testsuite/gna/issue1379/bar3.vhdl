entity foo3 is
  generic (
    LENGTH : natural
    );
  port (
    input : in bit_vector(LENGTH - 1 downto 0)
    );
end foo3;

architecture behave of foo3 is
begin
end behave;

entity bar3 is
end entity bar3;

architecture behave of bar3 is
begin

  my_foo : entity work.foo3
    port map (
      input => (others => '0')
      );
end behave;
