--  GHDL Run Time (GRT) - C interface.
--  Copyright (C) 2005 Tristan Gingold
--
--  GHDL is free software; you can redistribute it and/or modify it under
--  the terms of the GNU General Public License as published by the Free
--  Software Foundation; either version 2, or (at your option) any later
--  version.
--
--  GHDL is distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
--  for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with GCC; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.

--  This package declares C types.
--  It is a really stripped down version of interfaces.C!
with System;

package Grt.C is
   pragma Preelaborate (Grt.C);

   --  Type void * and char *.
   subtype voids is System.Address;
   subtype chars is System.Address;
   subtype long is Long_Integer;

   --  Type size_t.
   type size_t is mod 2 ** Standard'Address_Size;

   --  Type int.  It is an alias on Integer for simplicity.
   subtype int is Integer;

   --  Low level memory management.
   procedure Free (Addr : System.Address);
   function Malloc (Size : size_t) return System.Address;
   function Realloc (Ptr : System.Address; Size : size_t)
                    return System.Address;

private
   pragma Import (C, Free);
   pragma Import (C, Malloc);
   pragma Import (C, Realloc);
end Grt.C;
