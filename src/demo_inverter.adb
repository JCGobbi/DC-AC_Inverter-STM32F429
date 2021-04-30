with Ada.Real_Time; use Ada.Real_Time;

with STM_Board;     use STM_Board;
with Inverter_ADC;  use Inverter_ADC;
with Inverter_PWM;  use Inverter_PWM;

with Last_Chance_Handler; pragma Unreferenced (Last_Chance_Handler);
--  The "last chance handler" is the user-defined routine that is called when
--  an exception is propagated. We need it in the executable, therefore it
--  must be somewhere in the closure of the context clauses.

procedure Demo_Inverter is
--  This demonstration program only initializes the GPIOs and PWM timer and
--  presents on the output of the full-bridge a sinusoidal wave of 60 Hz.
--  There is no initialization for ADC and timer, so there is no analog monitoring.

begin

   --  Initialize GPIO ports
   Initialize_GPIO;

   --  Select the AC frequency of the inverter
   PWM_Frequency_Hz := 30_000.0;

   --  Select gain = 1.0 to see only sine table sinusoid
   Sine_Gain := 1.0;

   --  Disable PWM gate drivers because some gate drivers enable with
   --  low level.
   Set_PWM_Gate_Power (False);

   --  Initialize PWM generator
   Initialize_PWM (Frequency => PWM_Frequency_Hz,
                   Deadtime  => PWM_Deadtime,
                   Alignment => Center);

   for P in PWM_Phase'Range loop
      Set_Duty_Cycle (This => P,
                      Value => 0.0);
      Enable_Phase (P);
   end loop;

   --  Test if all peripherals are correctly initialized
   while not (STM_Board.Is_Initialized and Inverter_PWM.Is_Initialized)  loop
         Turn_On (Red_LED);
         delay until Clock + Milliseconds (1000);  --  arbitrary
         Turn_Off (Red_LED);
         delay until Clock + Milliseconds (1000);  --  arbitrary
   end loop;

   --  Enable PWM gate drivers
   Set_PWM_Gate_Power (True);

   --  Start generating the sinusoid
   Start_PWM;

   --  Enter steady state
   loop
      Set_Toggle (Green_LED);
      delay until Clock + Milliseconds (1000);  -- arbitrary
   end loop;

end Demo_Inverter;