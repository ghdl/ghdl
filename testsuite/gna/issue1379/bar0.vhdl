entity foo is
  generic (
    LENGTH : natural
    );
  port (
    input : in bit_vector(LENGTH - 1 downto 0)
    );
end foo;

architecture behave of foo is
begin
end behave;

entity bar is
end entity bar;

architecture behave of bar is
  component foo is
  port (
    input : in bit_vector(7 downto 0)
    );
  end component;

begin

  my_foo : foo
    port map (
      input => (others => '0')
      );
end behave;

configuration cfg of bar is
  for cfg
  end for;
end cfg;
