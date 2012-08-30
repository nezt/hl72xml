hl72xml
=======

Convert HL7 message to xml format

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