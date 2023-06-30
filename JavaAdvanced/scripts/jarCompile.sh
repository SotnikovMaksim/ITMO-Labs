DEP="../../java-advanced-2023/artifacts/info.kgeorgiy.java.advanced.hello.jar"
FILE="../java-solutions/info/kgeorgiy/ja/sotnikov/hello/HelloUDPServer.java"
WORKING_DIR="../jar"

javac -cp "$DEP" "$FILE" -d "$WORKING_DIR"
jar -cfm "$WORKING_DIR/HelloUDPServer.jar" MANIFEST.MF "$WORKING_DIR/info/kgeorgiy/ja/sotnikov/hello/HelloUDPServer.class"
