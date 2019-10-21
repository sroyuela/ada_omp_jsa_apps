--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                           P A R A L L E L .
--                P R O C E D U R A L _ R E D U C T I O N .
--                          R E C U R S I O N
--                                                                          --
--                                S p e c                                   --
--                                                                          --
--                  Copyright (C) 2011, Bradley J. Moore                    --
--                                                                          --
--  Paraffin is free software;  you can  redistribute it  and/or modify it  --
--  under  terms of the  GNU General Public License  as  published  by the  --
--  Free Software  Foundation;  either version 3,  or (at your option) any  --
--  later  version.  Paraffin is  distributed in the hope that it  will be  --
--  useful, but WITHOUT ANY WARRANTY; without even the implied warranty of  --
--  MERCHANTABILITY  or  FITNESS  FOR  A  PARTICULAR PURPOSE.  See the GNU  --
--  General Public License for  more details.  You should have  received a  --
--  copy of the GNU General Public License distributed with Paraffin;  see  --
--  file  COPYING.  If  not,  write  to  the  Free  Software  Foundation,   --
--  51 Franklin  Street,  Fifth  Floor, Boston, MA 02110-1301, USA.         --
--                                                                          --
--  As a  special exception, if other files  instantiate generics from      --
--  this unit,  or you link this  unit with other files  to produce an      --
--  executable,  this unit  does  not by  itself  cause the  resulting      --
--  executable to be covered by  the GNU General Public License.  This      --
--  exception does  not however invalidate  any other reasons  why the      --
--  executable file might be covered by the GNU Public License.             --
------------------------------------------------------------------------------

pragma Warnings (Off, "*Reducer* is not referenced");
pragma Warnings (Off, "*Identity_Value* is not referenced");

generic

   type Result_Type is private;
   --  Final Result type

   with procedure Reducer (Left, Right : in out Result_Type);
   --  Reducing operation used to compute final result. The operation
   --  needs to take two values and reduce into a single value (the Left)
   --  parameter. The Right parameter is in out to allow for finalization
   --  if necessary.

   Identity_Value : Result_Type;
   --  A special value that when applied as the right operand of the
   --  Reducing function, does not change the value of the left operand.

   type Work_Type is private;
   --  Data type to be processed recursively

package Parallel.Procedural_Reducing_Recursion is

   type Parallelism_Manager is limited interface;

   procedure Execute_Parallel_Subprogram
     (Manager : in out Parallelism_Manager;
      Item : Work_Type;
      Worker_Count : Worker_Count_Type := Default_Worker_Count;
      --  Top level item to process recursively
      Process : not null access
        procedure (Item : Work_Type;
                   Result : out Result_Type);
      Result : out Result_Type)
   is abstract;

end Parallel.Procedural_Reducing_Recursion;

pragma Warnings (On, "*Reducer* is not referenced");
pragma Warnings (On, "*Identity_Value* is not referenced");
