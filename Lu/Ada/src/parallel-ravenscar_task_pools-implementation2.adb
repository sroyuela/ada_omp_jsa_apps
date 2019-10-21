package body Parallel.Ravenscar_Task_Pools.Implementation2 is
   protected body Worker_Manager is

      procedure Offer_Work
        (Worker : Pool_Index;
         Plan   : not null Work_Plan_Access;
         Item   : Plan_Index) is
      begin
         --  Cant make another offer until the previous one has been accepted
         pragma Assert (not Offered_Work);

         Worker_Index := Worker;
         Assignment_Id   := Item;
         Assignment_Plan := Plan;
         Offered_Work := True;
      end Offer_Work;

      entry Wait_For_Work
        (Worker : out Pool_Index;
         Plan   : out Work_Plan_Access;
         Item   : out Plan_Index) when Offered_Work is
      begin
         Worker := Worker_Index;
         Plan   := Assignment_Plan;
         Item   := Assignment_Id;
         Assignment_Id := 0;
         Offered_Work := False;
      end Wait_For_Work;

   end Worker_Manager;

   protected body Task_Manager is

      procedure Next_Worker_Id
        (Plan : not null Work_Plan_Access;
         Item : out Plan_Index) is
      begin
         --  Let the plan safely initialize its state as this is
         --  called from within a protected operation.
         Plan.Starting (Item);

      end Next_Worker_Id;

      procedure Offer_Work
        (Plan          : not null Work_Plan_Access;
         Item          : Plan_Index) is
      begin

         Find_Worker_Loop : loop

            if Last_Worker = Number_Of_Workers then
               Last_Worker := 1;
            else
               Last_Worker := Last_Worker + 1;
            end if;

            exit Find_Worker_Loop when Idle (Last_Worker);

         end loop Find_Worker_Loop;

         Idle (Last_Worker) := False;

         Pool.Workers (Last_Worker).Manager.Offer_Work
           (Last_Worker,
            Plan,
            Item);

      end Offer_Work;

      procedure Offer_Work_To_Group
       (Plan : not null Work_Plan_Access;
        Worker_Count : Positive_Worker_Count) is
      begin
         for I in 1 .. Worker_Count loop

            Find_Worker_Loop : loop

               if Last_Worker = Number_Of_Workers then
                  Last_Worker := 1;
               else
                  Last_Worker := Last_Worker + 1;
               end if;

               exit Find_Worker_Loop when Idle (Last_Worker);

            end loop Find_Worker_Loop;

            Idle (Last_Worker) := False;

            Pool.Workers (Last_Worker).Manager.Offer_Work
              (Last_Worker,
               Plan,
               Plan_Index (I));
         end loop;
      end Offer_Work_To_Group;

      procedure Set_Worker_Idle
        (Worker : Pool_Index;
         Plan : not null Work_Plan_Access;
         Item : Plan_Index) is
      begin
         Idle (Worker) := True;
         Plan.Completing (Item);
      end Set_Worker_Idle;

   end Task_Manager;

   task body Worker_Task is
      Worker_Index : Pool_Index;
      Plan   : Work_Plan_Access;
      Item   : Plan_Index;
   begin -- Worker

      Work_Loop : loop

         --  Execution completed, look for work
         --  from other tasks if possible
         Static_Worker.Manager.Wait_For_Work
           (Worker_Index,
            Plan,
            Item);

         Plan.Engage (Worker_Index, Item);

      end loop Work_Loop;

   end Worker_Task;

   procedure Finished_Work
     (Pool : in out Task_Pool;
      Worker : Pool_Index;
      Plan : aliased in out Work_Plan'Class;
      Item : Plan_Index) is
   begin
      Pool.Manager.Set_Worker_Idle (Worker,
                                    Plan'Unchecked_Access,
                                    Item);
   end Finished_Work;

   procedure Next_Worker_Id
     (Pool : in out Task_Pool;
      Plan : aliased in out Work_Plan'Class;
      Item : out Plan_Index) is
   begin
      Pool.Manager.Next_Worker_Id (Plan'Unchecked_Access, Item);
   end Next_Worker_Id;

   procedure Offer_Work
     (Pool : in out Task_Pool;
      Plan : aliased in out Work_Plan'Class;
      Item : Plan_Index) is
   begin
      Pool.Manager.Offer_Work (Plan'Unchecked_Access, Item);
   end Offer_Work;

   procedure Offer_Work_To_Group
     (Pool : in out Task_Pool;
      Plan : aliased in out Work_Plan'Class;
      Worker_Count : Positive_Worker_Count) is
   begin
      Pool.Manager.Offer_Work_To_Group (Plan'Unchecked_Access, Worker_Count);
   end Offer_Work_To_Group;

   function Priority
     (Pool : Task_Pool) return System.Priority is
      pragma Unreferenced (Pool);
   begin
      return Worker_Priority;
   end Priority;

end Parallel.Ravenscar_Task_Pools.Implementation2;
