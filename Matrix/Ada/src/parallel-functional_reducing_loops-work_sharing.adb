------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                            P A R A L L E L .
--                F U N C T I O N A L _ R E D U C T I O N .
--                              L O O P S .
--                        W O R K _ S H A R I N G                           --
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
with Ada.Exceptions;

--  with Parallel.Iteration;
with Parallel.Functional_Reducing_Linked_List;

package body Parallel.Functional_Reducing_Loops.Work_Sharing is

   function Create
     (Priority : System.Priority := Dynamic_Priorities.Get_Priority;
      Storage_Size : System.Storage_Elements.Storage_Count :=
        Default_Worker_Storage_Size)
      return Work_Sharing_Manager is
   begin
      return Manager : Work_Sharing_Manager
        (Priority, Storage_Size)
      do
         null;
      end return;

   end Create;

   procedure Execute_Parallel_Loop
     (Manager : Work_Sharing_Manager;
      From : Iteration_Index_Type := Iteration_Index_Type'First;
      To : Iteration_Index_Type := Iteration_Index_Type'Last;
      Worker_Count : Worker_Count_Type := Use_Optimal_Worker_Count;
      Process   : not null access procedure
        (Start, Finish : Iteration_Index_Type;
         Item : in out Result_Type);
      Result : in out Result_Type)
   is

      Iterations : constant Positive := Positive'Base
        (Iteration_Index_Type'Pos (To) - Iteration_Index_Type'Pos (From)) + 1;

      --  If the amount of work < number of workers, then set the worker
      --  count to match the amount of work
      Effective_Workers : constant Positive_Worker_Count :=
        Effective_Worker_Count
          (Workers    => Worker_Count,
           Iterations => Iterations);

      subtype Effective_Worker_Id is Worker_Id range 1 .. Effective_Workers;

      package Reducing_List is new Functional_Reducing_Linked_List
        (Result_Type,
         Reducer,
         Identity_Value);

      Reduction_List : Reducing_List.List :=
        Reducing_List.Create (Worker_Count => Effective_Workers,
                              Priority => Manager.Ceiling_Priority);

      protected Work_Sharing_Manager is

         pragma Priority (Manager.Ceiling_Priority);

         procedure Check_Completion;
         procedure Save_Exception (E : Exceptions.Exception_Occurrence);

      private
         Exception_Raised    : Boolean := False;
         Saved_Exception     : Ada.Exceptions.Exception_Occurrence;
      end Work_Sharing_Manager;

      protected body Work_Sharing_Manager is

         procedure Check_Completion is
         begin
            if Exception_Raised then
               Ada.Exceptions.Reraise_Occurrence (X => Saved_Exception);
            end if;
         end Check_Completion;

         procedure Save_Exception (E : Exceptions.Exception_Occurrence) is
         begin
            if not Exception_Raised then
               Exception_Raised := True;
               Ada.Exceptions.Save_Occurrence (Target => Saved_Exception,
                                               Source => E);
            end if;
         end Save_Exception;

      end Work_Sharing_Manager;

      task type Worker
        (Work_Id : Effective_Worker_Id := Effective_Worker_Id'First;
         Start_Index,
         End_Index : Iteration_Index_Type := Iteration_Index_Type'First)
      is
         pragma Storage_Size (Manager.Storage_Size);
         pragma Priority (Manager.Ceiling_Priority);
      end Worker;

      task body Worker is
         Value : Result_Type := Identity_Value;
      begin

         Process (Start_Index, End_Index, Value);

         Reducing_List.Reduce
           (Container => Reduction_List,
            Item    => Value,
            Position  => Reducing_List.To_Cursor (Worker => Work_Id));

      exception
         when E : others =>
            Work_Sharing_Manager.Save_Exception (E);
      end Worker;

      Start_Index         : Iteration_Index_Type := From;
      End_Index           : Iteration_Index_Type;

      Iterations_Per_Task : constant Positive :=
        Iterations / Positive (Effective_Workers);

      Remainder           : Natural :=
        Iterations rem Positive (Effective_Workers);

      Reduction_Result : Result_Type;

      Next_Worker : Effective_Worker_Id := Effective_Worker_Id'First;

      function Create_Worker return Worker is
      begin

         if Remainder = 0 then
            End_Index := Iteration_Index_Type'Val
              (Iteration_Index_Type'Pos (Start_Index) +
               (Iterations_Per_Task - 1));
         else
            End_Index := Iteration_Index_Type'Val
              (Iteration_Index_Type'Pos (Start_Index) + Iterations_Per_Task);

            Remainder := Remainder - 1;
         end if;

         return New_Worker : Worker
           (Work_Id => Next_Worker,
            Start_Index => Start_Index,
            End_Index => End_Index)
         do

            if Next_Worker < Effective_Worker_Id'Last then
               Next_Worker := Next_Worker + 1;
               Start_Index := Iteration_Index_Type'Succ (End_Index);
            end if;
         end return;
      end Create_Worker;

      Workers : constant array (Effective_Worker_Id'Range) of Worker :=
        (others => Create_Worker);
      pragma Unreferenced (Workers);

   begin

      --  Blocking call until reduction is complete
      Reducing_List.Result (Reduction_List, Reduction_Result);

      --  Reduce the initial value into the result
      Result := Reducer (Result, Reduction_Result);

      Work_Sharing_Manager.Check_Completion;

   end Execute_Parallel_Loop;

   ---------------------------------------------------

   function Get_Worker_Id
     (Manager : Work_Sharing_Manager;
      From : Iteration_Index_Type := Iteration_Index_Type'First;
      To : Iteration_Index_Type := Iteration_Index_Type'Last;
      Worker_Count : Worker_Count_Type := Use_Optimal_Worker_Count;
      Iteration : Iteration_Index_Type) return Worker_Id
   is
      pragma Unreferenced (Manager);
      Iterations : constant Positive := Positive'Base
        (Iteration_Index_Type'Pos (To) - Iteration_Index_Type'Pos (From)) + 1;

      --  If the amount of work < number of workers, then set the worker
      --  count to match the amount of work
      Effective_Workers : constant Positive_Worker_Count :=
        Effective_Worker_Count
          (Workers    => Worker_Count,
           Iterations => Iterations);

      Iterations_Per_Task : constant Positive :=
        Iterations / Positive (Effective_Workers);

      Remainder           : constant Natural :=
        Iterations rem Positive (Effective_Workers);

      Chunk_Size : constant Integer := Iterations_Per_Task +
        (if Remainder = 0 then 0 else 1);

      Normalized_Index : constant Natural :=
        Iteration_Index_Type'Pos (Iteration) - Iteration_Index_Type'Pos (From);

      Worker : constant Worker_Id
        := Worker_Id (Normalized_Index / Chunk_Size + 1);

   begin -- Get_Worker_Id
      return Worker;
   end Get_Worker_Id;

end Parallel.Functional_Reducing_Loops.Work_Sharing;
