--
-- create a mask with all length bit set


library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;


entity len_to_mask is
port
(
 len: in unsigned;
 mask: out std_logic_vector
);
end entity;
 
architecture len_to_mask_rtl of len_to_mask is

constant size: integer := mask'length;

begin

--
-- result in a mask such as:
-- mask(mask'length - 1 downto len) = '0';
-- mask(len - 1 downto 0) = '1';

process(len)
begin
 for i in 2 to size loop
  if i = to_integer(len) then
   mask(size - 1 downto i) <= (others => '0');
   mask(i - 1 downto 0) <= (others => '1');
  end if;
 end loop;
end process;

end len_to_mask_rtl;


--
-- lsb to msb conversion


library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;


entity lsb_to_msb is
port
(
 en: in std_logic;
 data_len: in unsigned;
 lsb_data: in std_logic_vector;
 msb_data: out std_logic_vector
);
end entity;
 
architecture lsb_to_msb_rtl of lsb_to_msb is

constant size: integer := lsb_data'length;

begin

--
-- lsb_data: uuuuLxxxxM
-- msb_data: uuuuMxxxxL
-- where u undefined, x 0 or 1

process(en, lsb_data)
begin

 if (en = '1') then

  for i in 1 to size loop
   if i = to_integer(data_len) then
    msb_data(msb_data'length - 1 downto i) <= (others => '0');
    for j in 0 to (i - 1) loop
     msb_data(j) <= lsb_data(i - 1 - j);
    end loop;
    exit ;
   end if;
  end loop;

 else

  msb_data <= lsb_data;

 end if;

end process;

end lsb_to_msb_rtl;


--
-- binary to gray conversion


library ieee;
use ieee.std_logic_1164.all;


entity bin_to_gray is
port
(
 -- enable conversion
 en: in std_logic;
 bin_data: in std_logic_vector;
 gray_data: out std_logic_vector
);
end entity;
 
architecture bin_to_gray_rtl of bin_to_gray is

constant size: integer := bin_data'length;

begin

assert (bin_data'length = gray_data'length)
report "size differs" severity failure;

process(en, bin_data) is
begin
 if en = '1' then
  for j in 0 to (size - 2) loop
   gray_data(j) <= bin_data(j) xor bin_data(j + 1);
  end loop;
  gray_data(size - 1) <= bin_data(size - 1);
 else
  gray_data <= bin_data;
 end if;
end process;

end bin_to_gray_rtl;


--
-- gray to binary conversion


library ieee;
use ieee.std_logic_1164.all;


entity gray_to_bin is
port
(
 -- enable conversion
 en: in std_logic;
 gray_data: in std_logic_vector;
 bin_data: out std_logic_vector
);
end entity;
 
architecture gray_to_bin_rtl of gray_to_bin is

constant size: integer := bin_data'length;

signal tmp_data: std_logic_vector(bin_data'range);

begin

assert (bin_data'length = gray_data'length)
report "size differs" severity failure;

process(en, gray_data, tmp_data) is
begin
 if (en = '1') then
  for j in 0 to (size - 2) loop
   tmp_data(j) <= gray_data(j) xor tmp_data(j + 1);
  end loop;
  tmp_data(size - 1) <= gray_data(size - 1);
  bin_data <= tmp_data;
 else
  bin_data <= gray_data;
 end if;
end process;

end gray_to_bin_rtl;


--
-- extend sign


library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;


entity extend_sign is
port
(
 data_len: in unsigned;
 data_in: in std_logic_vector;
 data_out: out std_logic_vector;
 len_mask: in std_logic_vector
);
end entity;
 
architecture extend_sign_rtl of extend_sign is

constant size: integer := data_in'length;
signal is_signed: std_logic;

begin

process(data_in, data_len)
begin
 -- fixme: modelsim fails without this check
 -- synthesis translate_off
 is_signed <= '0';
 if data_len > 0 then
 -- synthesis translate_on

 is_signed <= data_in(to_integer(data_len) - 1);
 
 -- synthesis translate_off
 end if;
 -- synthesis translate_on

end process;

process(is_signed, data_in)
begin
 if (is_signed = '1') then
  data_out <= data_in or not len_mask;
 else
  data_out <= data_in;
 end if;
end process;

end extend_sign_rtl;
