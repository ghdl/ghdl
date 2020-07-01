entity ex1 is
        generic (
        g_use_dsp          : STRING   := "YES"
        );
	attribute use_dsp : string;
	attribute use_dsp of ex1: entity is g_use_dsp;
end entity;

architecture a of ex1 is
begin
  process begin
    report "Hello world" severity note;
    wait;
  end process;
end a;
