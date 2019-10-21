------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                            P A R A L L E L .
--                P R O C E D U R A L _ R E D U C T I O N .
--                              L O O P S .
--                        W O R K _ S E E K I N G                           --
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
with Parallel.Procedural_Reducing_Linked_List;

package body Parallel.Procedural_Reducing_Loops.Work_Seeking is

   function Create
     (Chunk_Size : Natural := Default_Chunk_Size;
      Ceiling_Priority : System.Priority :=
        Dynamic_Priorities.Get_Priority;
      Storage_Size : System.Storage_Elements.Storage_Count :=
        Default_Worker_Storage_Size)
      return Work_Seeking_Manager is
   begin
      return Manager : Work_Seeking_Manager
        (Chunk_Size,
         Ceiling_Priority,
         Storage_Size)
      do
         null;
      end return;

   end Create;

   overriding procedure Execute_Parallel_Loop
     (Manager : Work_Seeking_Manager;
      From : Iteration_Index_Type := Iteration_Index_Type'First;
      To   : Iteration_Index_Type := Iteration_Index_Type'Last;
      Worker_Count : Worker_Count_Type := Use_Optimal_Worker_Count;
      Process   : not null access procedure
        (Start, Finish : Iteration_Index_Type;
         Item : in out Result_Type);
      Result : aliased in out Result_Type)
   is

      Other_Workers : Parallel.Work_Seeking_State :=
        (Seeking_Work => False);

      Iterations : constant Positive := Positive'Base
        (Iteration_Index_Type'Pos (To) - Iteration_Index_Type'Pos (From)) + 1;

      Local_Minimum_Seek : constant Natural :=
        Determine_Chunk_Size
          (Workers    =>  Worker_Count_Type (Number_Of_CPUs),
           Iterations => Iterations,
           Requested_Chunk_Size => Manager.Chunk_Size);

      Work_Chunks : constant Positive := Iterations / Local_Minimum_Seek +
        Boolean'Pos (Iterations mod Local_Minimum_Seek > 0);

      --  If the amount of work < number of workers, then set the worker
      --  count to match the amount of work
      Effective_Workers : constant Positive_Worker_Count :=
        Effective_Worker_Count
          (Workers    => Worker_Count,
           Iterations => Work_Chunks);

      subtype Effective_Worker_Id is Worker_Id range 1 .. Effective_Workers;

      package Reducing_List is new Procedural_Reducing_Linked_List
        (Result_Type,
         Reducer,
         Identity_Value);

      Reduction_List : Reducing_List.List :=
        Reducing_List.Create (Worker_Count => Effective_Workers,
                              Priority     => Manager.Ceiling_Priority);

      protected Internal_Manager is

         pragma Priority (Manager.Ceiling_Priority);

         entry Request_Work
           (Start, Finish : out Iteration_Index_Type;
            Seeker : Effective_Worker_Id;
            Done : out Boolean);

         procedure Offer_Work
           (Start : Iteration_Index_Type;
            Finish : in out Iteration_Index_Type;
            Donor : Effective_Worker_Id;
            Work_Accepted : out Boolean);

         procedure Save_Exception (E : Exceptions.Exception_Occurrence);

         procedure Check_Completion;

      private
         entry Wait_For_Later_Offer
           (Start, Finish : out Iteration_Index_Type;
            Seeker : Effective_Worker_Id;
            Done : out Boolean);

         entry Wait_For_Work
           (Start, Finish : out Iteration_Index_Type;
            Seeker : Effective_Worker_Id;
            Done : out Boolean);

         Exception_Raised  : Boolean := False;
         Saved_Exception   : Ada.Exceptions.Exception_Occurrence;
         Work_Offered      : Boolean := False;
         All_Work_Complete : Boolean := False;
         Assignment_Start, Assignment_Finish : Iteration_Index_Type;
         Next_Waiting_Worker : Effective_Worker_Id;
      end Internal_Manager;

      protected body Internal_Manager is

         procedure Check_Completion is
         begin
            if Exception_Raised then
               Ada.Exceptions.Reraise_Occurrence (X => Saved_Exception);
            end if;
         end Check_Completion;

         entry Request_Work
           (Start,
            Finish : out Iteration_Index_Type;
            Seeker : Effective_Worker_Id;
            Done : out Boolean) when Standard.True is
         begin

            if Wait_For_Work'Count + Wait_For_Later_Offer'Count =
              Effective_Workers - 1
              or else All_Work_Complete then
               All_Work_Complete := True;
               Done := True;
            else
               All_Work_Complete := False;

               Other_Workers := (Seeking_Work => True);
               if Wait_For_Work'Count = 0 then
                  Next_Waiting_Worker := Seeker;
                  requeue Wait_For_Work;
               else
                  requeue Wait_For_Later_Offer;
               end if;
            end if;
         end Request_Work;

         entry Wait_For_Later_Offer
           (Start, Finish : out Iteration_Index_Type;
            Seeker : Effective_Worker_Id;
            Done : out Boolean)
           when Wait_For_Work'Count = 0 or else All_Work_Complete is
            --  Workers queue here if there is already a worker queued
            --  on the Wait_For_Work entry
         begin
            if All_Work_Complete then
               Done := True;
            else
               Next_Waiting_Worker := Seeker;
               requeue Wait_For_Work;
            end if;
         end Wait_For_Later_Offer;

         pragma Warnings (Off, "*Seeker* is not referenced");

         entry Wait_For_Work
           (Start, Finish : out Iteration_Index_Type;
            Seeker : Effective_Worker_Id;
            Done : out Boolean)
           when Work_Offered or else All_Work_Complete is
            pragma Unreferenced (Seeker);
         begin
            if All_Work_Complete then
               Done := True;
            else
               Done := False;
               Start := Assignment_Start;
               Finish := Assignment_Finish;
               Work_Offered := False;
            end if;
         end Wait_For_Work;

         pragma Warnings (On, "*Seeker* is not referenced");

         procedure Offer_Work
           (Start : Iteration_Index_Type;
            Finish : in out Iteration_Index_Type;
            Donor : Effective_Worker_Id;
            Work_Accepted : out Boolean) is
         begin
            pragma Warnings
              (Off, "*comparison on unordered enumeration*");

            --  Others looking for work and
            --  offerer has more than one iteration left
            if Wait_For_Work'Count > 0 and then Finish > Start then

               Work_Offered := True;

               --  Insert the new node into the reduction list
               Reducing_List.Insert_Right
                 (Container => Reduction_List,
                  Item      => Reducing_List.To_Cursor
                    (Worker => Next_Waiting_Worker),
                  Position  => Reducing_List.To_Cursor
                    (Worker => Donor));

               Assignment_Finish := Finish;
               Finish :=
                 Iteration_Index_Type'Val
                   (Iteration_Index_Type'Pos (Start) +
                    ((Iteration_Index_Type'Pos (Finish) -
                         Iteration_Index_Type'Pos (Start)) / 2));

               Assignment_Start := Iteration_Index_Type'Succ (Finish);
               --  Are others still looking for work?
               Other_Workers := (Seeking_Work => (Wait_For_Work'Count > 1));
               Work_Accepted := True;
            else
               --  Nobody looking for work or no work to offer
               Work_Accepted := False;
            end if;

            pragma Warnings
              (On, "*comparison on unordered enumeration*");

            --  Do nothing if nobody is waiting for work (or there is only one
            --  iteration remaining.
            --  The Offering task will need to be restarted using the values
            --  for Start and Finish
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
        (Work_Id : Effective_Worker_Id := Effective_Worker_Id'First;
         Start_Index,
         End_Index : Iteration_Index_Type := Iteration_Index_Type'First)
      is
         pragma Storage_Size (Manager.Storage_Size);
         pragma Priority (Manager.Ceiling_Priority);
      end Worker;

      task body Worker is
         First_Index : Iteration_Index_Type := Start_Index;
         Last_Index : Iteration_Index_Type := End_Index;
         Previous_Last_Index : Iteration_Index_Type;
         Value               : Result_Type := Identity_Value;
         Done : Boolean := False;
         Work_Accepted : Boolean := False;

         procedure Iteration
           (Start  : Iteration_Index_Type;
            Finish : in out Iteration_Index_Type;
            Item : in out Result_Type)
         is
            Chunk_Start, Chunk_Finish : Iteration_Index_Type;
         begin
            pragma Warnings
              (Off, "*subrange of unordered enumeration*");

            for I in Start .. Finish loop

               Chunk_Start := Iteration_Index_Type'Val
                 (Iteration_Index_Type'Pos (From) +
                  (Iteration_Index_Type'Pos (I) - 1) * Local_Minimum_Seek);

               if Iteration_Index_Type'Pos (I) = Work_Chunks then
                  Chunk_Finish := To;
               else
                  Chunk_Finish := Iteration_Index_Type'Val
                    (Iteration_Index_Type'Pos (Chunk_Start)
                     + Local_Minimum_Seek - 1);
               end if;

               Process (Chunk_Start, Chunk_Finish, Item);

               if Other_Workers.Seeking_Work then
                  Other_Workers := (Seeking_Work => False);
                  Finish        := I;
                  exit;
               end if;
            end loop;

            pragma Warnings
              (On, "*subrange of unordered enumeration*");

         end Iteration;

      begin

         Previous_Last_Index := Last_Index;

         Work_Loop : while not Done loop

            Iteration (First_Index, Last_Index, Value);

            if Last_Index /= Previous_Last_Index then

               --  Execution cut short,
               --  another task must be trying to Seek work
               Internal_Manager.Offer_Work
                 (Start => Iteration_Index_Type'Succ (Last_Index),
                  Finish => Previous_Last_Index,
                  Donor => Work_Id,
                  Work_Accepted => Work_Accepted);

               First_Index := Iteration_Index_Type'Succ (Last_Index);
               Last_Index := Previous_Last_Index;
            else

               --  Now do the real reduction
               Reducing_List.Reduce
                 (Container => Reduction_List,
                  Item    => Value,
                  Position  => Reducing_List.To_Cursor
                    (Worker => Work_Id));

               --  Execution completed, look for work
               --  from other tasks if possible
               Internal_Manager.Request_Work
                 (Start     => First_Index,
                  Finish    => Last_Index,
                  Seeker    => Work_Id,
                  Done => Done);

               if not Done then
                  --  Resetting Value, since we are starting a new work task
                  Value := Identity_Value;
                  Previous_Last_Index := Last_Index;
               end if;
            end if;
         end loop Work_Loop;

      exception
         when E : others =>
            Internal_Manager.Save_Exception (E);
      end Worker;

      Start_Index      : Iteration_Index_Type := Iteration_Index_Type'Val (1);
      End_Index        : Iteration_Index_Type;

      Iterations_Per_Task : constant Positive :=
        Work_Chunks / Positive (Effective_Workers);

      Remainder           : Natural :=
        Work_Chunks rem Positive (Effective_Workers);

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

   begin

      declare
         Workers : constant array (Effective_Worker_Id'Range) of Worker
           := (others => Create_Worker);
         pragma Unreferenced (Workers);
      begin
         null;
      end;

      --  Blocking call until reduction is complete.
      Reducing_List.Result (Reduction_List, Reduction_Result);

      Reducer (Result, Reduction_Result);
      pragma Unreferenced (Reduction_Result);

      Internal_Manager.Check_Completion;

   end Execute_Parallel_Loop;

end Parallel.Procedural_Reducing_Loops.Work_Seeking;
