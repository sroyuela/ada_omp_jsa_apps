------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                            P A R A L L E L                               --
--                                                                          --
--                                B o d y                                   --
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

with Ada.Environment_Variables;
use Ada;

package body Parallel is

   Worker_Count_Initialized : Boolean := False;
   Default_Workers : Positive_Worker_Count := 1;

   Debug_Initialized : Boolean := False;
   Debug_Mode : Boolean := False;

   function "<=" (L, R : Atomic_Loop_Index_Type) return Boolean is
   begin
      return L.Value <= R.Value;
   end "<=";

   function Debug_Logging return Boolean is
      Debugging_Enabled : constant String :=
        "PARALLEL_DEBUG_ENABLED";
   begin

      if not Debug_Initialized and then
        Environment_Variables.Exists
          (Name => Debugging_Enabled) then

         Debug_Mode := Boolean'Value
           (Environment_Variables.Value (Name => Debugging_Enabled));
         Debug_Initialized := True;
      end if;

      return Debug_Mode;

   end Debug_Logging;

   ----------------------------------------------------------------------

   function Default_Worker_Count return Positive_Worker_Count is
      Worker_Count : constant String := "DEFAULT_WORKER_COUNT";
   begin
      if not Worker_Count_Initialized then
         if Environment_Variables.Exists
           (Name => Worker_Count) then

            Default_Workers := Positive_Worker_Count'Value
              (Environment_Variables.Value (Name => Worker_Count));
            Worker_Count_Initialized := True;
         else
            Default_Workers := Positive_Worker_Count (Number_Of_CPUs);
         end if;

         Worker_Count_Initialized := True;
      end if;
      return Default_Workers;
   end Default_Worker_Count;

   ----------------------------------------------------------------

   function Determine_Chunk_Size
     (Workers : Worker_Count_Type := Use_Optimal_Worker_Count;
      Iterations : Positive;
      Requested_Chunk_Size : Natural := Default_Chunk_Size) return Positive
   is
      Effective_Workers : constant Positive_Worker_Count :=
        (if Workers = Use_Optimal_Worker_Count then
         Optimal_Worker_Count (Iterations) else Workers);
   begin

      if Requested_Chunk_Size = Default_Chunk_Size then
         return Natural'Max
           (1,
            Natural'Min
              (Iterations / Positive (Effective_Workers) / 4, 10000));
      else
         --  Client provided a value, so use that.
         return Requested_Chunk_Size;
      end if;
   end Determine_Chunk_Size;

   ----------------------------------------------------------------

   function Effective_Worker_Count
     (Workers : Worker_Count_Type;
      Iterations : Positive) return Positive_Worker_Count is
   begin
      if Workers = Use_Optimal_Worker_Count then
         return Optimal_Worker_Count (Iterations);
      else
         return Positive_Worker_Count
           (Positive'Min
              (Iterations, Positive (Workers)));
      end if;
   end Effective_Worker_Count;

   ----------------------------------------------------------------------

   procedure Next (Item : in out Atomic_Loop_Index_Type) is
   begin
      Item.Value := Item.Value + 1;
   end Next;

   ----------------------------------------------------------------------

   function Optimal_Worker_Count
     (Iterations : Positive)
      return Worker_Count_Type is
   begin
      return Worker_Count_Type
        (Positive'Min
           (Iterations,
            Positive (Number_Of_CPUs) +
            (Iterations mod Positive (Number_Of_CPUs))));
   end Optimal_Worker_Count;

end Parallel;
