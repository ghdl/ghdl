package gpkg2 is
  generic (const : natural);
end package;

package ipkg2 is new work.gpkg2 generic map (const => 1);

entity ent2 is
end entity;

architecture a of ent2 is
begin
  main : process
  begin
     -- Case 1
     assert work.ipkg.const = 1; -- Should this result in a 'no declaration of const' error?
     -- case 2     
     assert << constant @work.ipkg.const : natural>> = 1; -- Should this be visible?
  end process;
end architecture;
