library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

package rawfile_pack is
   constant elf32_magic         : unsigned := x"7F454C46"; -- 0x7f E L F
   constant elf32_type_mach     : unsigned := x"0002002a"; -- type 2, SuperH
   constant elf32_hdr_off       : integer  := 16;          -- skips the e_ident field
   constant elf32_phentsize_off : integer  := 42;          -- size of the segment records
   constant elf32_phnum_off     : integer  := 44;          -- pointer to the segment records

   constant elf32_pt_load       : unsigned := "00000001";

   alias byte is character;
   type byte_vector is array (positive range <>) of byte;
   type byte_f is file of byte;

   type rom8_t  is array (natural range <>) of std_logic_vector( 7 downto 0);
   type rom8_p  is access rom8_t;
   type rom16_t is array (natural range <>) of std_logic_vector(15 downto 0);
   type rom16_p  is access rom16_t;
   type rom32_t is array (natural range <>) of std_logic_vector(31 downto 0);
   type rom32_p  is access rom32_t;

   type rawfile is record
      fn   : line;
      offs : natural;
   end record;

   procedure fopen(file f : byte_f; r: inout rawfile; fn: string);
   procedure fseek(file f : byte_f; r: inout rawfile;  o: natural);
   procedure fread(file f : byte_f; r: inout rawfile;  b: inout byte);
   procedure fread(file f : byte_f; r: inout rawfile;  b: inout byte_vector; l: natural);
   procedure fread(file f : byte_f; r: inout rawfile;  n: inout natural; b: positive);
   procedure fread(file f : byte_f; r: inout rawfile;  n: inout natural);
   procedure fread(file f : byte_f; r: inout rawfile;  n: inout unsigned);
   procedure fclose(file f: byte_f; r: inout rawfile);

   impure function fread_bin(fn: string; l: positive) return rom8_t;
   impure function fread_bin(fn: string; l: positive) return rom16_t;
   impure function fread_bin(fn: string; l: positive) return rom32_t;

   impure function fread_elf(fn: string; l: positive) return rom8_t;
   impure function fread_elf(fn: string; l: positive) return rom16_t;
   impure function fread_elf(fn: string; l: positive) return rom32_t;
end package;

package body rawfile_pack is
   procedure fopen(file f: byte_f; r: inout rawfile; fn: string) is
   begin
      r.fn := new string'(fn);
      r.offs := 0;
      file_open(f, r.fn.all, read_mode);
   end fopen;

   procedure fseek(file f: byte_f; r: inout rawfile; o: natural) is
   variable b: byte;
   begin
      if o < r.offs then
         file_close(f);
         r.offs := 0;
         file_open(f, r.fn.all, read_mode);
      end if;

      for i in r.offs to o loop
         if i /= o then read(f, b); end if;
      end loop;
      r.offs := o;
   end fseek;

   procedure fread(file f: byte_f; r: inout rawfile; b: inout byte) is
   begin
      read(f, b);
      r.offs := r.offs + 1;
   end fread;

   procedure fread(file f: byte_f; r: inout rawfile; b: inout byte_vector; l: natural) is
   begin
      for i in 1 to l loop
         read(f, b(i));
      end loop;
      r.offs := r.offs + l;
   end fread;

   procedure fread(file f : byte_f; r: inout rawfile;  n: inout natural; b: positive) is
   variable o : positive := b/8;
   variable s : byte_vector(1 to o);
   variable v : natural := 0;
   begin
      fread(f, r, s, o);
      for i in 1 to o loop
         v := v*256 + byte'pos(s(i));
      end loop;

      n := v;
   end fread;

   procedure fread(file f: byte_f; r: inout rawfile; n: inout natural) is
   begin
      fread(f, r, n, 32);
   end fread;

   procedure fread(file f : byte_f; r: inout rawfile;  n: inout unsigned) is
   variable o : positive := (n'length-1)/8 + 1;
   variable s : byte_vector(1 to o);
   variable v : unsigned(o*8-1 downto 0) := (others => '0');
   begin
      fread(f, r, s, o);
      for i in 1 to o loop
         v := v(o*8-9 downto 0) & to_unsigned(byte'pos(s(i)), 8);
      end loop;

      n := v((n'length-1) downto 0);
   end fread;

   procedure fclose(file f: byte_f; r: inout rawfile) is
   begin
      file_close(f);
      deallocate(r.fn);
   end fclose;

   impure function fread_bin(fn: string; l: positive) return rom8_t is
   file f : byte_f;
   variable fd: rawfile;
   variable r : rom8_p;
   variable u : unsigned( 7 downto 0);
   begin
      r := new rom8_t(0 to l-1);
      r.all := (others => (others => '0'));

      fopen(f, fd, fn);
      for i in 0 to l loop
         exit when endfile(f);
         assert i<l report "ROM contents overrun" severity warning;
         fread(f, fd, u);
         r.all(i) := std_logic_vector(u);
      end loop;
      fclose(f, fd);

      return r.all;
   end fread_bin;

   impure function fread_bin(fn: string; l: positive) return rom16_t is
   file f : byte_f;
   variable fd: rawfile;
   variable r : rom16_p;
   variable u : unsigned(15 downto 0);
   begin
      r := new rom16_t(0 to l-1);
      r.all := (others => (others => '0'));

      fopen(f, fd, fn);
      for i in 0 to l loop
         exit when endfile(f);
         assert i<l report "ROM contents overrun" severity warning;
         fread(f, fd, u);
         r.all(i) := std_logic_vector(u);
      end loop;
      fclose(f, fd);

      return r.all;
   end fread_bin;

   impure function fread_bin(fn: string; l: positive) return rom32_t is
   file f : byte_f;
   variable fd: rawfile;
   variable r : rom32_p;
   variable u : unsigned(31 downto 0);
   begin
      r := new rom32_t(0 to l-1);
      r.all := (others => (others => '0'));

      fopen(f, fd, fn);
      for i in 0 to l loop
         exit when endfile(f);
         assert i<l report "ROM contents overrun" severity warning;
         fread(f, fd, u);
         r.all(i) := std_logic_vector(u);
      end loop;
      fclose(f, fd);

      return r.all;
   end fread_bin;

   impure function fread_elf(fn: string; l: positive) return rom8_t is
   file f      : byte_f;
   variable fd : rawfile;
   variable r  : rom8_p;
   variable v  : natural;
   variable u  : unsigned(31 downto 0);
   variable u8 : unsigned( 7 downto 0);

   variable e_phoff  : natural;
   variable e_phsize : natural;
   variable e_phnum  : natural;
   variable p_offset : natural;
   variable p_paddr  : natural;
   variable p_filesz : natural;
   begin
      r := new rom8_t(0 to l-1);
      r.all := (others => (others => '0'));

      fopen(f, fd, fn);
      fread(f, fd, u);
      assert u=elf32_magic report "Not an ELF file" severity failure;
      fseek(f, fd, elf32_hdr_off);
      fread(f, fd, u);
      assert u=elf32_type_mach report "ELF for unknown machine" severity warning;
      fread(f, fd, v); -- ELF file version
      fread(f, fd, u);
      fread(f, fd, e_phoff);       report "offset phdr:  " & natural'image(e_phoff);
      fseek(f, fd, elf32_phentsize_off);
      fread(f, fd, e_phsize, 16);  report "phdr size:    " & natural'image(e_phsize);
      fread(f, fd, e_phnum,  16);  report "num segments: " & natural'image(e_phnum);

      for i in 1 to e_phnum loop
         fseek(f, fd, e_phoff + e_phsize*(i-1));
         fread(f, fd, u);
         if u = elf32_pt_load then
            fread(f, fd, p_offset);
            fread(f, fd, v); -- p_vaddr
            assert v=0 report "Virtual segment addr  " & natural'image(v) severity warning;
            fread(f, fd, p_paddr);
            fread(f, fd, p_filesz);
            assert p_filesz < l   report "ROM contents overrun" severity failure;
            report "-LOAD segment addr " & natural'image(p_paddr) & " @offset " & natural'image(p_offset) & " size " & natural'image(p_filesz);

            fseek(f, fd, p_offset);
            for j in 0 to p_filesz-1 loop
               fread(f, fd, u8);
               r.all(j+p_paddr) := std_logic_vector(u8);
            end loop;
         else
            -- report "-Segment type " & hex(u);
         end if;
      end loop;
      fclose(f, fd);

      return r.all;
   end fread_elf;

   impure function fread_elf(fn: string; l: positive) return rom16_t is
   file f      : byte_f;
   variable fd : rawfile;
   variable r  : rom16_p;
   variable v  : natural;
   variable u  : unsigned(31 downto 0);
   variable u16: unsigned(15 downto 0);

   variable e_phoff  : natural;
   variable e_phsize : natural;
   variable e_phnum  : natural;
   variable p_offset : natural;
   variable p_paddr  : natural;
   variable p_filesz : natural;
   begin
      r := new rom16_t(0 to l-1);
      r.all := (others => (others => '0'));

      fopen(f, fd, fn);
      fread(f, fd, u);
      assert u=elf32_magic report "Not an ELF file" severity failure;
      fseek(f, fd, elf32_hdr_off);
      fread(f, fd, u);
      assert u=elf32_type_mach report "ELF for unknown machine" severity warning;
      fread(f, fd, v); -- ELF file version
      fread(f, fd, u);
      fread(f, fd, e_phoff);       report "offset phdr:  " & natural'image(e_phoff);
      fseek(f, fd, elf32_phentsize_off);
      fread(f, fd, e_phsize, 16);  report "phdr size:    " & natural'image(e_phsize);
      fread(f, fd, e_phnum,  16);  report "num segments: " & natural'image(e_phnum);

      for i in 1 to e_phnum loop
         fseek(f, fd, e_phoff + e_phsize*(i-1));
         fread(f, fd, u);
         if u = elf32_pt_load then
            fread(f, fd, p_offset);
            fread(f, fd, v); -- p_vaddr
            assert v=0 report "Virtual segment addr  " & natural'image(v) severity warning;
            fread(f, fd, p_paddr);
            fread(f, fd, p_filesz);
            assert p_filesz < l*2 report "ROM contents overrun" severity failure;
            report "-LOAD segment addr " & natural'image(p_paddr) & " @offset " & natural'image(p_offset) & " size " & natural'image(p_filesz);

            fseek(f, fd, p_offset);
            for j in 0 to (p_filesz+1)/2-1 loop
               fread(f, fd, u16);
               r.all(j+p_paddr/2) := std_logic_vector(u16);
            end loop;
         else
            -- report "-Segment type " & hex(u);
         end if;
      end loop;
      fclose(f, fd);

      return r.all;
   end fread_elf;

   impure function fread_elf(fn: string; l: positive) return rom32_t is
   file f      : byte_f;
   variable fd : rawfile;
   variable r  : rom32_p;
   variable v  : natural;
   variable u  : unsigned(31 downto 0);
   variable u32: unsigned(31 downto 0);

   variable e_phoff  : natural;
   variable e_phsize : natural;
   variable e_phnum  : natural;
   variable p_offset : natural;
   variable p_paddr  : natural;
   variable p_filesz : natural;
   begin
      r := new rom32_t(0 to l-1);
      r.all := (others => (others => '0'));

      fopen(f, fd, fn);
      fread(f, fd, u);
      assert u=elf32_magic report "Not an ELF file" severity failure;
      fseek(f, fd, elf32_hdr_off);
      fread(f, fd, u);
      assert u=elf32_type_mach report "ELF for unknown machine" severity warning;
      fread(f, fd, v); -- ELF file version
      fread(f, fd, u);
      fread(f, fd, e_phoff);       report "offset phdr:  " & natural'image(e_phoff);
      fseek(f, fd, elf32_phentsize_off);
      fread(f, fd, e_phsize, 16);  report "phdr size:    " & natural'image(e_phsize);
      fread(f, fd, e_phnum,  16);  report "num segments: " & natural'image(e_phnum);

      for i in 1 to e_phnum loop
         fseek(f, fd, e_phoff + e_phsize*(i-1));
         fread(f, fd, u);
         if u = elf32_pt_load then
            fread(f, fd, p_offset);
            fread(f, fd, v); -- p_vaddr
            assert v=0 report "Virtual segment addr  " & natural'image(v) severity warning;
            fread(f, fd, p_paddr);
            fread(f, fd, p_filesz);
            assert p_filesz < l*4 report "ROM contents overrun" severity failure;
            report "-LOAD segment addr " & natural'image(p_paddr) & " @offset " & natural'image(p_offset) & " size " & natural'image(p_filesz);

            fseek(f, fd, p_offset);
            for j in 0 to (p_filesz+3)/4-1 loop
               fread(f, fd, u32);
               r.all(j+p_paddr/4) := std_logic_vector(u32);
            end loop;
         else
            -- report "-Segment type " & hex(u);
         end if;
      end loop;
      fclose(f, fd);

      return r.all;
   end fread_elf;

end rawfile_pack;
