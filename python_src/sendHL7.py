#!/usr/bin/env python
# -*- coding: utf-8 -*-

import socket
import sys
import string
import random

def getPort():
    return str(random.randrange(20000,55000))

def getString(size):
   return join(random.choice('1234567890abcde') for x in range(size))
#UDP
#s = socket.socket(socket.AF_INET,socket.SOCK_DGRAM)
#TCP
s = socket.socket(socket.AF_INET,socket.SOCK_STREAM)
# Estableciendo a 5 Segundos como máximo para recibir respuesta.
s.settimeout(5)

host = "192.168.24.182"
source = "192.168.1.10"
puerto = 7070

print 'Servidor Destino: (host,puerto) : ',(host,puerto)

# Nos conectamos con el servidor
s.connect((host,puerto))

portSIP=getPort()

# Mensaje que se enviará

mensaje  =  "MSH|^~\&|Xxxxx|SENDING FACILITY|RAMSOFT|RECEIVING FACILITY|20101223202939-0400||ADT^A40|102|P|2.3.1||||||||\n"
mensaje +=  "EVN|A40|20101223202939-0400||||\n"
mensaje +=  "PID||P12345^^^ISSUER|P12345^^^ISSUER||PATIENT^TEST^M^^^^||19741018|M|||10808 FOOTHILL BLVD^^PANCHO MUCAMONGA^CA^91730^US||(909)481-5872^^^sales@Xxxx.com|(909)481-5800x1||M||12345|286-50-9510|||\n"
mensaje +=  "MRG|758026^^^ISSUER|||758026^^^ISSUER|\n"




# Envío la informacióm
s.send(mensaje)

print ':::::>Finalizando la conexión'
s.close()

