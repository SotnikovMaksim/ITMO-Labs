package md2html;

import java.io.*;
import java.lang.StringBuilder;
import java.nio.charset.StandardCharsets;


public class Md2Html {
    private static char[] buffer;
    private static BufferedReader reader;
    private static BufferedWriter writer;
    private static int position = 0;
    private static boolean eol = false;
    private static boolean eof = false;
    private static String line;
    private static boolean flag = false;

    private static final char ASTERISK = "*".charAt(0);
    private static final char QUOTE = "`".charAt(0);
    private static final char SHARP = "#".charAt(0);
    private static final char HYPHEN = "-".charAt(0);
    private static final char UNDERLINE = "_".charAt(0);
    private static final char SPACE = " ".charAt(0);
    private static final char NEW_LINE = System.lineSeparator().charAt(0);
    private static final char LEFT_BRACKET = "<".charAt(0);
    private static final char RIGHT_BRACKET = ">".charAt(0);
    private static final char AMPERSAND = "&".charAt(0);
    private static final char BACKSLASH = "\\".charAt(0);
    private static final char PERCENT = "%".charAt(0);

    public static void main(String[] args) {
        try {
            reader = new BufferedReader(new InputStreamReader(new FileInputStream(args[0]), StandardCharsets.UTF_8));
            writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(args[1]), StandardCharsets.UTF_8));
            line = reader.readLine();
        } catch (IOException e) {
            eof = true;
            e.getStackTrace();
        }
        try {
            while (!eof) {
                if (line.isEmpty()) {
                    while (reader.ready()) {
                        line = reader.readLine();
                        if (!line.isEmpty()) {
                            break;
                        }
                    }
                    if (line.isEmpty()) {
                        break;
                    }
                }
                StringBuilder sb = new StringBuilder();
                input(line);
                if (buffer[position] == SHARP) {
                    header(sb);
                } else {
                    text(sb);
                }
                writer.write(sb.toString());
                writer.newLine();
                if (!reader.ready()) {
                    eof = true;
                }
            }
        } catch (IOException e) {
            eof = true;
        }
        try {
            writer.close();
            reader.close();
        } catch (IOException e) {
            e.getStackTrace();
        }
        reader = null;
        writer = null;
        eof = false;
        position = 0;
    }

    private static void header(StringBuilder sb) {
        int counter = 0;
        while (buffer[position] == SHARP) {
            counter += 1;
            move(1);
        }
        try {
            if (buffer[position] == SPACE) {
                move(1);
                sb.append("<h").append(counter).append(">");
                general_markup(sb, "");
                while (reader.ready()) {
                    if (emptyLine(sb, "")) {
                        break;
                    }
                }
                sb.append("</h").append(counter).append(">");
            } else {
                text(sb.append("#".repeat(counter)));
            }
        } catch (IOException e) {
            eof = true;
        }
    }

    private static void text(StringBuilder sb) {
        sb.insert(0, "<p>");
        general_markup(sb, "");
        try {
            while (reader.ready()) {
                if (emptyLine(sb, "")) {
                    break;
                }
            }
            sb.append("</p>");
        } catch (IOException e) {
            eof = true;
        }
    }

    private static void general_markup(StringBuilder sb, String parameter) {
        boolean closed = false;
        while (!eol) {
            if (buffer[position] == ASTERISK) {
                if ((position + 1 < buffer.length) && (buffer[position + 1] == ASTERISK))
                    if (parameter.equals("strong*")) {
                        closed = true;
                        move(2);
                        break;
                    } else {
                        strong(sb);
                    }
                else {
                    if (parameter.equals("em*")) {
                        closed = true;
                        move(1);
                        break;
                    } else {
                        emphasis(sb);
                    }
                }
            } else if (buffer[position] == UNDERLINE) {
                if ((position + 1 < buffer.length) && (buffer[position + 1] == UNDERLINE))
                    if (parameter.equals("strong_")) {
                        closed = true;
                        move(2);
                        break;
                    } else {
                        strong(sb);
                    }
                else {
                    if (parameter.equals("em_")) {
                        closed = true;
                        move(1);
                        break;
                    } else {
                        emphasis(sb);
                    }
                }
            } else if (buffer[position] == HYPHEN && buffer[position + 1] == HYPHEN) {
                if (parameter.equals("s ")) {
                    closed = true;
                    move(2);
                    break;
                } else {
                    strikeout(sb);
                }
            } else if (buffer[position] == QUOTE) {
                if (parameter.equals("code ")) {
                    closed = true;
                    move(1);
                    break;
                } else {
                    code(sb);
                }
            } else if (buffer[position] == PERCENT) {
                if (parameter.equals("var ")) {
                    closed = true;
                    move(1);
                    break;
                } else {
                    variable(sb);
                }
            } else if (buffer[position] == LEFT_BRACKET) {
                sb.append("&lt;");
                move(1);
            } else if (buffer[position] == AMPERSAND) {
                sb.append("&amp;");
                move(1);
            } else if (buffer[position] == RIGHT_BRACKET) {
                sb.append("&gt;");
                move(1);
            } else if (buffer[position] == BACKSLASH) {
                sb.append(buffer[position + 1]);
                move(2);
            } else {
                sb.append(buffer[position]);
                move(1);
            }
        }
        flag = closed;
    }

    private static void local_markup(StringBuilder sb, String parameter, int marker_length) {
        StringBuilder symbol = new StringBuilder();
        for (int i = 0; i < marker_length; i++) {
            symbol.append(buffer[position]);
            move(1);
        }
        StringBuilder content = new StringBuilder();
        general_markup(content, parameter);
        try {
            while (!flag && reader.ready()) {
                if (emptyLine(content, parameter)) {
                    break;
                }
            }
        } catch (IOException e) {
            eof = true;
        }
        if (flag) {
            sb.append("<").append(parameter, 0, parameter.length() - 1).append(">").append(content).append("</").append(parameter, 0, parameter.length() - 1).append(">");
        } else {
            sb.append(symbol).append(content);
        }
    }

    private static void strong(StringBuilder in) {
        if (buffer[position] == UNDERLINE) {
            local_markup(in, "strong_", 2);
        } else {
            local_markup(in, "strong*", 2);
        }
    }

    private static void emphasis(StringBuilder in) {
        if (buffer[position] == ASTERISK) {
            local_markup(in, "em*", 1);
        } else {
            local_markup(in, "em_", 1);
        }
    }

    private static void strikeout(StringBuilder in) {
        local_markup(in, "s ", 2);
    }

    private static void code(StringBuilder in) {
        local_markup(in, "code ", 1);
    }

    private static void variable(StringBuilder in) {
        local_markup(in, "var ", 1);
    }

    private static boolean emptyLine(StringBuilder sb, String parameter) throws IOException {
        line = reader.readLine();
        if (line.isEmpty()) {
            return true;
        } else {
            sb.append(NEW_LINE);
            input(line);
            general_markup(sb, parameter);
            return false;
        }
    }

    private static void input(String line) {
        if (!eof) {
            buffer = line.toCharArray();
            position = 0;
            eol = false;
        }
    }

    private static void move(int step) {
        if (!eol) {
            position += step;
            if (position >= buffer.length) {
                eol = true;
            }
        }
    }
}