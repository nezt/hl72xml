Hl72xml
=======

Introduction
====

Health Level Seven International (HL7) is the global authority on standards 
for interoperability of health information technology .

General Message Format
====

    * All HL7 messages begin with \x0B (ASCII 11) and terminate with
      \x1C (ASCII 28) and \x0D (ASCII 13).
    * Each message segment ends with the carriage return character (\x0D, ASCII 13).
    * Field sequences in the message segments are separated by “|” (\xC0, ASCII 124).
    * Field components are separated by “^” (\x5E, ASCII 94).
    * Field sub-components are separated by “&” (\x26, ASCII 38).
    * Repeated fields are separated by “~” (\x7E, ASCII 126).
	
Message HL7 example
=====

	MSH|^~\&|GHH LAB|ELAB-3|GHH OE|BLDG4|200202150930||ORU^R01|CNTRL-3456|P|2.4<cr>
	PID|||555-44-4444||EVERYWOMAN^EVE^E^^^^L|JONES|19620320|F|||153 FERNWOOD DR.^
	^STATESVILLE^OH^35292||(206)3345232|(206)752-121||||AC555444444||67-A4335^OH^20030520<cr>
	OBR|1|845439^GHH OE|1045813^GHH LAB|15545^GLUCOSE|||200202150730|||||||||
	555-55-5555^PRIMARY^PATRICIA P^^^^MD^^|||||||||F||||||444-44-4444^HIPPOCRATES^HOWARD H^^^^MD<cr>
	OBX|1|SN|1554-5^GLUCOSE^POST 12H CFST:MCNC:PT:SER/PLAS:QN||^182|mg/dl|70_105|H|||F<cr>

with this proyect you can convert HL7 message to xml format

	* How to start

Clone the project:

			$ git clone https://github.com/nezt/hl72xml.git
			
Into hl72xml:

			$ cd hl72xml
			
Let's compile and start the application:

			$ make compile && make start
			
Note: You can configure the port used by app in env section of .app file

To test the application execute the script in phyton placed in python_src directory
configure the ip and port to connect:

			...
			host = "192.168.24.182"
			source = "192.168.1.10"	
			puerto = 7070
			...

And the message in hl7 will be displayed as xml format in erlang shell, enjoy it!! 
