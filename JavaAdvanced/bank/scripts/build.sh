#!/bin/bash
SERVER_DIR=".."

javac --class-path .. "${SERVER_DIR}/Server.java" "${SERVER_DIR}/Client.java" "${SERVER_DIR}/BankWebServer.java"
#rmic -d $CLASSPATH examples.rmi.RemoteAccount examples.rmi.RemoteBank
