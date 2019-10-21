------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                            P A R A L L E L .
--                F U N C T I O N A L _ R E D U C T I O N .
--                              L O O P S .
--                       W O R K _ S T E A L I N G                           --
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
with Ada.Numerics.Discrete_Random; use Ada.Numerics;

with Parallel.Functional_Reducing_Linked_List;

package body Parallel.Functional_Reducing_Loops.Work_Stealing is

   function Create
     (Chunk_Size : Natural := Default_Chunk_Size;
      Ceiling_Priority : System.Priority :=
        Dynamic_Priorities.Get_Priority;
      Storage_Size : System.Storage_Elements.Storage_Count :=
        Default_Worker_Storage_Size)
      return Work_Stealing_Manager is
   begin
      return Manager : Work_Stealing_Manager
        (Chunk_Size,
         Ceiling_Priority,
         Storage_Size)
      do
         null;
      end return;

   end Create;

   overriding procedure Execute_Parallel_Loop
     (Manager : Work_Stealing_Manager;
      From : Iteration_Index_Type := Iteration_Index_Type'First;
      To   : Iteration_Index_Type := Iteration_Index_Type'Last;
      Worker_Count : Worker_Count_Type := Use_Optimal_Worker_Count;
      Process   : not null access procedure
        (Start, Finish : Iteration_Index_Type;
         Item : in out Result_Type);
      Result : in out Result_Type)
   is

      Iterations : constant Positive := Positive'Base
        (Iteration_Index_Type'Pos (To) - Iteration_Index_Type'Pos (From)) + 1;

      Local_Minimum_Steal : constant Natural :=
        Determine_Chunk_Size
          (Workers    =>  Worker_Count_Type (Number_Of_CPUs),
           Iterations => Iterations,
           Requested_Chunk_Size => Manager.Chunk_Size);

      Work_Chunks : constant Positive := Iterations / Local_Minimum_Steal +
        Boolean'Pos (Iterations mod Local_Minimum_Steal > 0);

      --  If the amount of work < number of workers, then set the worker
      --  count to match the amount of work
      Effective_Workers : constant Positive_Worker_Count :=
        Effective_Worker_Count
          (Workers    => Worker_Count,
           Iterations => Work_Chunks);

      subtype Effective_Worker_Id is Worker_Id range 1 .. Effective_Workers;

      type Worker_Progress_Type is
         record
            Iterator : aliased Atomic_Loop_Index_Type;
            Last_Index : aliased Atomic_Loop_Index_Type;
         end record;

      package Random_Worker is new Discrete_Random
        (Result_Subtype => Effective_Worker_Id);

      Random_Worker_Generator : Random_Worker.Generator;

      type Worker_Progress_Array is
        array (Effective_Worker_Id) of Worker_Progress_Type;
      Worker_Progress : Worker_Progress_Array;
      pragma Volatile (Worker_Progress);

      package Reducing_List is new Functional_Reducing_Linked_List
        (Result_Type,
         Reducer,
         Identity_Value);

      Reduction_List : Reducing_List.List :=
        Reducing_List.Create (Worker_Count => Effective_Workers,
                              Priority     => Manager.Ceiling_Priority);

      type Idle_State_Array is array (Effective_Worker_Id) of Boolean;

      protected Internal_Manager is

         pragma Priority (Manager.Ceiling_Priority);

         entry Wait_For_Workers_To_Start;

         entry Request_Work
           (Start, Finish : out Work_Stealing_Loop_Index;
            Stealer : Effective_Worker_Id;
            Done : out Boolean);

         procedure Offer_Work
           (Start, Finish : Work_Stealing_Loop_Index;
            Donor : Effective_Worker_Id);

         procedure Save_Exception (E : Exceptions.Exception_Occurrence);
         procedure Check_Completion;

      private

         entry Wait_For_Later_Offer
           (Start, Finish : out Work_Stealing_Loop_Index;
            Stealer : Effective_Worker_Id;
            Done : out Boolean);

         entry Wait_For_Work
           (Start, Finish : out Work_Stealing_Loop_Index;
            Stealer : Effective_Worker_Id;
            Done : out Boolean);

         Exception_Raised    : Boolean := False;
         Saved_Exception     : Ada.Exceptions.Exception_Occurrence;
         Work_Offered : Boolean := False;
         All_Work_Complete : Boolean := False;
         Assignment_Start, Assignment_Finish : Work_Stealing_Loop_Index;
         Initial_Victim : Worker_Count_Type;
         Current_Victim : Effective_Worker_Id'Base := 0;
         Next_Waiting_Worker : Effective_Worker_Id;
         Idlers : Idle_State_Array := (others => False);
         Release_The_Hounds : Boolean := False;
      end Internal_Manager;

      protected body Internal_Manager is

         procedure Check_Completion is
         begin
            if Exception_Raised then
               Ada.Exceptions.Reraise_Occurrence (X => Saved_Exception);
            end if;
         end Check_Completion;

         entry Wait_For_Workers_To_Start
           when Wait_For_Workers_To_Start'Count =
             Natural (Effective_Worker_Id'Last) or else All_Work_Complete
         or else Release_The_Hounds is
         begin
            Release_The_Hounds := True;
         end Wait_For_Workers_To_Start;

         entry Request_Work
           (Start, Finish : out Work_Stealing_Loop_Index;
            Stealer : Effective_Worker_Id;
            Done : out Boolean) when Standard.True is
         begin
            Idlers (Stealer) := True;

            if Wait_For_Work'Count + Wait_For_Later_Offer'Count =
              Effective_Workers - 1
              or else All_Work_Complete then

               All_Work_Complete := True;
               Done := True;
            elsif Wait_For_Work'Count = 0 then
               Next_Waiting_Worker := Stealer;
               Initial_Victim := 0;
               requeue Wait_For_Work;
            else
               requeue Wait_For_Later_Offer;
            end if;
         end Request_Work;

         entry Wait_For_Later_Offer
           (Start, Finish : out Work_Stealing_Loop_Index;
            Stealer : Effective_Worker_Id;
            Done : out Boolean)
           when Wait_For_Work'Count = 0 or else All_Work_Complete is
            --  Workers queue here if there is already a worker queued
            --  on the Wait_For_Work entry
         begin
            if All_Work_Complete then
               Done := True;
            else
               Next_Waiting_Worker := Stealer;
               Initial_Victim := 0;
               requeue Wait_For_Work;
            end if;
         end Wait_For_Later_Offer;

         entry Wait_For_Work
           (Start, Finish : out Work_Stealing_Loop_Index;
            Stealer : Effective_Worker_Id;
            Done : out Boolean)
           when Initial_Victim = 0 or else
                Work_Offered or else
                Idlers (Current_Victim) or else
                All_Work_Complete is
         begin
            Done := False;

            if All_Work_Complete then
               Done := True;
            else
               if Initial_Victim = 0 then

                  Initial_Victim := Random_Worker.Random
                        (Gen => Random_Worker_Generator);
                  Current_Victim := Initial_Victim;

               elsif Work_Offered then
                  Start := Assignment_Start;
                  Finish := Assignment_Finish;
                  Work_Offered := False;
                  Idlers (Stealer) := False;
                  Current_Victim := 0;
                  return;

               elsif Idlers (Current_Victim) then
                  Current_Victim :=
                    (Current_Victim rem Effective_Worker_Id'Last) + 1;
                  if Current_Victim = Initial_Victim then
                     Done := True;
                  end if;
               end if;

               while not Done loop
                  if Current_Victim /= Stealer then

                     if Worker_Progress (Current_Victim).Iterator.Value <
                       Worker_Progress (Current_Victim).Last_Index.Value
                       - Local_Minimum_Steal then

                        --  Attempt to cause the worker to exit early
                        --  by tricking it into thinking it has completed
                        --  its iterations.
                        Worker_Progress (Current_Victim).Last_Index
                           := Worker_Progress (Current_Victim).Iterator;

                        --  Wait for the Victim to either
                        --  offer work, or request more work
                        --  if it escaped.
                        requeue Wait_For_Work;
                     end if;
                  end if;

                  --  Still looking? Advance to next worker
                  Current_Victim :=
                    (Current_Victim rem Effective_Worker_Id'Last) + 1;

                  if Current_Victim = Initial_Victim then
                     Done := True;
                  end if;
               end loop;

            end if;
         end Wait_For_Work;

         procedure Offer_Work
           (Start, Finish : Work_Stealing_Loop_Index;
            Donor : Effective_Worker_Id) is
         begin

            Assignment_Start := Start;
            Assignment_Finish := Finish;
            Work_Offered := True;

            --  Insert the new node into the reduction list
            Reducing_List.Insert_Right
              (Container => Reduction_List,
               Item      => Reducing_List.To_Cursor
                 (Worker => Next_Waiting_Worker),
               Position  => Reducing_List.To_Cursor
                 (Worker => Donor));

         end Offer_Work;

         procedure Save_Exception (E : Exceptions.Exception_Occurrence) is
         begin
            if not Exception_Raised then
               Exception_Raised := True;
               Ada.Exceptions.Save_Occurrence (Target => Saved_Exception,
                                               Source => E);
               All_Work_Complete := True;
            end if;
         end Save_Exception;

      end Internal_Manager;

      task type Worker
        (Work_Id : Effective_Worker_Id := Effective_Worker_Id'First)
      is
         pragma Storage_Size (Manager.Storage_Size);
         pragma Priority (Manager.Ceiling_Priority);
      end Worker;

      task body Worker is
         Previous_Last_Index : Work_Stealing_Loop_Index
           := Worker_Progress (Work_Id).Last_Index.Value;
         Temp : Work_Stealing_Loop_Index;
         Value   : Result_Type := Identity_Value;
         Done : Boolean := False;

         procedure Iteration
           (Iter   : in out Atomic_Loop_Index_Type;
            Finish : Atomic_Loop_Index_Type;
            Item    : in out Result_Type)
         is
            Chunk_Start, Chunk_Finish : Iteration_Index_Type;
         begin
            while Iter <= Finish loop

               Chunk_Start := Iteration_Index_Type'Val
                 (Iteration_Index_Type'Pos (From) +
                  (Iter.Value - 1) * Local_Minimum_Steal);

               if Iter.Value = Work_Chunks then
                  Chunk_Finish := To;
               else
                  Chunk_Finish := Iteration_Index_Type'Val
                    (Iteration_Index_Type'Pos (Chunk_Start)
                     + Local_Minimum_Steal - 1);
               end if;

               Process (Chunk_Start, Chunk_Finish, Item);

               Parallel.Next (Iter);
            end loop;
         end Iteration;

      begin

         Internal_Manager.Wait_For_Workers_To_Start;

         Work_Loop : loop

            Iteration (Worker_Progress (Work_Id).Iterator,
                       Worker_Progress (Work_Id).Last_Index,
                       Value);

            if Worker_Progress (Work_Id).Last_Index.Value
              /= Previous_Last_Index then

               Worker_Progress (Work_Id).Last_Index.Value :=
                 (Previous_Last_Index -
                    Worker_Progress (Work_Id).Iterator.Value) / 2 +
                 Worker_Progress (Work_Id).Iterator.Value;

               --  Need to set Previous_Last_Index before we make an Offer
               --  because someone might steal work before we get to store
               --  the Previous_Last_Index otherwise.
               Temp := Previous_Last_Index;
               Previous_Last_Index :=
                 Worker_Progress (Work_Id).Last_Index.Value;

               --  Execution cut short,
               --  another task must be trying to Seek work
               Internal_Manager.Offer_Work
                 (Start => Work_Stealing_Loop_Index'Succ
                   (Worker_Progress (Work_Id).Last_Index.Value),
                  Finish => Temp,
                  Donor => Work_Id);

            else

               pragma Assert
                 (Worker_Progress (Work_Id).Iterator.Value >=
                  Worker_Progress (Work_Id).Last_Index.Value);

               --  Now do the real reduction
               Reducing_List.Reduce
                 (Container => Reduction_List,
                  Item    => Value,
                  Position  => Reducing_List.To_Cursor
                    (Worker => Work_Id));

               --  Execution completed, look for work
               --  from other tasks if possible
               Internal_Manager.Request_Work
                 (Start     => Worker_Progress (Work_Id).Iterator.Value,
                  Finish    => Previous_Last_Index,
                  Stealer => Work_Id,
                  Done => Done);

               exit Work_Loop when Done;

               --  Resetting Value, since we are starting a new work task
               Value := Identity_Value;
            end if;
         end loop Work_Loop;

      exception
         when E : others =>
            Internal_Manager.Save_Exception (E);
            Reducing_List.Cancel (Reduction_List);
      end Worker;

      Start_Index         : Work_Stealing_Loop_Index := 1;
      End_Index           : Work_Stealing_Loop_Index;

      Iterations_Per_Task : constant Positive :=
        Work_Chunks / Positive (Effective_Workers);

      Remainder           : Natural :=
        Work_Chunks rem Positive (Effective_Workers);

      Reduction_Result : Result_Type;

      Next_Worker : Effective_Worker_Id := Effective_Worker_Id'First;

      function Create_Worker return Worker is
      begin
         if Remainder = 0 then
            End_Index := Start_Index + Iterations_Per_Task - 1;
         else
            End_Index := Start_Index + Iterations_Per_Task;

            Remainder := Remainder - 1;
         end if;

         Worker_Progress (Next_Worker) :=
           (Iterator => (Value => Start_Index),
            Last_Index => (Value => End_Index));

         return New_Worker : Worker
           (Work_Id => Next_Worker)
         do
            if Next_Worker < Effective_Worker_Id'Last then
               Next_Worker := Next_Worker + 1;
               Start_Index := Integer'Succ (End_Index);
            end if;
         end return;
      end Create_Worker;

   begin

      declare
         Workers : constant array (Effective_Worker_Id'Range) of Worker
           := (others => Create_Worker);
         pragma Unreferenced (Workers);
      begin
         null;
      end;

      --  Blocking call until reduction is complete
      Reducing_List.Result (Reduction_List, Reduction_Result);

      Result := Reducer (Result, Reduction_Result);

      Internal_Manager.Check_Completion;

   end Execute_Parallel_Loop;

end Parallel.Functional_Reducing_Loops.Work_Stealing;
