------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                             P A R A L L E L .
--                            R E C U R S I O N .
--                          W O R K _ S H A R I N G
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
with System;
with Ada.Exceptions; use Ada;

package body Parallel.Recursion.Work_Sharing is

   function Create
     (Ceiling_Priority : System.Priority := System.Default_Priority;
      Storage_Size : System.Storage_Elements.Storage_Count :=
        Default_Worker_Storage_Size)
      return Work_Sharing_Manager is
   begin
      return Manager : Work_Sharing_Manager
        (Ceiling_Priority,
         Storage_Size)
      do
         null;
      end return;

   end Create;

   procedure Execute_Parallel_Subprogram
     (Manager : in out Work_Sharing_Manager;
      Item : Work_Type;
      Worker_Count : Worker_Count_Type := Default_Worker_Count;
      Process : not null access procedure (Item : Work_Type))
   is

      protected type Internal_Work_Manager is

         pragma Priority (Manager.Ceiling_Priority);

         entry Wait_For_Work
           (Item : out Work_Type;
            Subs : out Worker_Count_Type;
            Done : out Boolean);

         procedure Offer_Work
           (Item          : Work_Type;
            Subs          : Worker_Count_Type;
            Work_Accepted : out Boolean);

         entry Wait_For_Worker_Initialization
           (Item          : Work_Type;
            Work_Accepted : out Boolean);

         procedure Check_Completion;

         procedure Worker_Done;

         procedure Save_Exception (E : Exceptions.Exception_Occurrence);

      private
         All_Done           : Boolean           := False;
         Outstanding_Workers : Worker_Count_Type := Worker_Count;
         Work_Offered       : Boolean           := False;
         Assignment_Value   : Work_Type;
         Assignment_Subs    : Worker_Count_Type := 0;
         Exception_Raised    : Boolean := False;
         Saved_Exception     : Ada.Exceptions.Exception_Occurrence;
      end Internal_Work_Manager;

      protected body Internal_Work_Manager is

         procedure Check_Completion is
         begin
            if Exception_Raised then
               Ada.Exceptions.Reraise_Occurrence (X => Saved_Exception);
            end if;
         end Check_Completion;

         procedure Offer_Work
           (Item          : Work_Type;
            Subs          : Worker_Count_Type;
            Work_Accepted : out Boolean) is
         begin
            --  Others looking for work
            if Wait_For_Work'Count > 0 then

               Work_Offered := True;
               Assignment_Value := Item;
               Assignment_Subs := Subs;

               Work_Accepted := True;
            else
               --  Nobody looking for work or no work to offer
               Work_Accepted := False;
            end if;
         end Offer_Work;

         procedure Save_Exception (E : Exceptions.Exception_Occurrence) is
         begin
            if not Exception_Raised then
               Ada.Exceptions.Save_Occurrence (Target => Saved_Exception,
                                               Source => E);
               Exception_Raised := True;
            end if;

            Outstanding_Workers := Outstanding_Workers - 1;
            if Wait_For_Work'Count = Outstanding_Workers then
               All_Done := True;
            end if;
         end Save_Exception;

         entry Wait_For_Work
           (Item : out Work_Type;
            Subs : out Worker_Count_Type;
            Done : out Boolean) when Work_Offered or else All_Done is
         begin
            Item := Assignment_Value;
            Subs := Assignment_Subs;

            Work_Offered := False;
            Done := All_Done;
         end Wait_For_Work;

         entry Wait_For_Worker_Initialization
           (Item          : Work_Type;
            Work_Accepted : out Boolean)
           when Wait_For_Work'Count = Worker_Count is
         begin
            Offer_Work (Item, Worker_Count, Work_Accepted);
         end Wait_For_Worker_Initialization;

         procedure Worker_Done is
         begin
            Outstanding_Workers := Outstanding_Workers - 1;
            if Wait_For_Work'Count = Outstanding_Workers then
               All_Done := True;
            end if;

         end Worker_Done;

      end Internal_Work_Manager;

      --  Allow client to call recursion routine
      Internal_Manager : Internal_Work_Manager;

      task type Worker is
         pragma Storage_Size (Manager.Storage_Size);
         pragma Priority (Manager.Ceiling_Priority);
      end Worker;

      task body Worker is
         Value          : Work_Type;
         Subcontractors : Worker_Count_Type;
         Done : Boolean;

      begin -- Worker

         --  Execution completed, look for work
         --  from other tasks if possible
         Internal_Manager.Wait_For_Work
           (Item => Value,
            Subs => Subcontractors,
            Done => Done);

         if not Done then
            Parallel_Task_Attributes.Set_Value (Val => Subcontractors);
            Process (Value);
         end if;

         Internal_Manager.Worker_Done;

      exception
         when E : others =>
            Internal_Manager.Save_Exception (E);
      end Worker;

      type Internal_Dispatcher_Type is new
        Recursion_Dispatcher with null record;

      overriding
      procedure Recurse
        (Dispatcher : Internal_Dispatcher_Type;
         Item       : Work_Type;
         Split      : Recursion_Branch_Count;
         Of_Splits  : Recursion_Branch_Count)
      is
         pragma Unreferenced (Dispatcher);

         Work_Accepted : Boolean := False;

         Subcontractors : constant Worker_Count_Type
           := Parallel_Task_Attributes.Value;

         --  Check to see if we can further divide the work
         Subcontractor_Count : Worker_Count_Type :=
           Subcontractors / Worker_Count_Type (Of_Splits);
      begin

         --  Assign any leftover subcontractors
         if Worker_Count_Type (Split) <=
           Subcontractors rem Worker_Count_Type (Of_Splits) then
            Subcontractor_Count := Subcontractor_Count + 1;
         end if;

         if Subcontractors > 0 then

            --  The last split is run under the current task, rather than
            --  attempt to offer to a new worker
            if Split = Of_Splits then

               Parallel_Task_Attributes.Set_Value (Val => Subcontractor_Count);

               --  If there is only one subcontractor left, this worker takes
               --  the work, and proceeds sequentially. Otherwise, there is
               --  still potential for farming out work to subcontractors,
               --  so we proceed in parallel.
               Process (Item);
               return;

            else
               Internal_Manager.Offer_Work
                 (Item,
                  Subcontractor_Count,
                  Work_Accepted);

               if Work_Accepted then
                  return;
               end if;  --  Work_Accepted
            end if; --  Split = Of_Splits
         end if; --  Workers_Are_Idle ...

         Process (Item);
      end Recurse;

      --  Allow client to call recursion routine
      Internal_Dispatcher : aliased Internal_Dispatcher_Type;

      Work_Accepted : Boolean;

   begin -- Execute_Parallel_Subprogram

      --  Allow client to call recursion routine
      Manager.Dispatcher := Internal_Dispatcher'Unchecked_Access;

      declare
         type Worker_Array is
           array (Positive_Worker_Count range <>) of Worker;

         Workers : Worker_Array (1 .. Worker_Count);
         pragma Unreferenced (Workers);
      begin

         Internal_Manager.Wait_For_Worker_Initialization
           (Item,
            Work_Accepted);
      end;

      pragma Assert (Work_Accepted);

      Internal_Manager.Check_Completion;

      Manager.Dispatcher := null;

   end Execute_Parallel_Subprogram;

end Parallel.Recursion.Work_Sharing;
