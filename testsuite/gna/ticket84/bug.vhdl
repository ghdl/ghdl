use std.textio.all;

entity ent is
end entity;

architecture a of ent is
begin
  main : process
    type binary_file is file of character;
    file fptr_text : text;
    file fptr_binary : binary_file;
  begin
    file_open(fptr_text, "file.txt", write_mode);
    write(fptr_text, string'("a"));
    write(fptr_text, string'("b"));
    file_close(fptr_text);

    file_open(fptr_binary, "file.raw", write_mode);
    write(fptr_binary, character'('a'));
    write(fptr_binary, character'('b'));
    file_close(fptr_binary);

    write(output, string'("a"));
    write(output, string'("b"));
    wait;
  end process;
end architecture;
